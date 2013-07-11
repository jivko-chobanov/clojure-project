(ns city-traffic.core)

(def running true)
(def dim 20)
(def dim-- (dec dim))
(def car-length 1)
(def min-road-length (* 2 car-length))
(def max-road-length (* 5 car-length))
(def min-road-tilt (* 3 car-length))
(def max-road-tilt-deviation (* 9 car-length))

(def animation-sleep-ms 100)
(def car-sleep-ms 400)

(def world
  (mapv (fn [y]
          (mapv (fn [x] (ref {:x x :y y}))
                (range dim)))
        (range dim)))

(defn place [[x y]]
  (-> world (nth y) (nth x)))

(defn delta-loc [[x y] dir]
  (case dir
    0 [[x (inc y)] [(dec x) (inc y)] [(inc x) (inc y)]]
    2 [[(inc x) y] [(inc x) (dec y)] [(inc x) (inc y)]]
    4 [[x (dec y)] [(dec x) (dec y)] [(inc x) (dec y)]]
    6 [[(dec x) y] [(dec x) (dec y)] [(dec x) (inc y)]]))

(defn move [old-place new-place]
  (let [car (:car @old-place)]
    (alter old-place dissoc :car)
    (alter new-place assoc :car car)))

(defn behave [car coordinates]
  (when running
    (let [p (place coordinates)
          ahead-coordinate-pairs-in-board (filter
                                            (fn [ahead-coordinates]
                                              (every? #(and (>= % 0) (< % dim)) ahead-coordinates))
                                            (delta-loc coordinates (:dir car)))
          ahead-coordinate-pair-with-crossing (first (filter
                                                       (fn [ahead-coordinates]
                                                         (let [ahead-info @(place ahead-coordinates)]
                                                           (or (:crossing ahead-info) (:tr-light ahead-info))))
                                                       ahead-coordinate-pairs-in-board))
          ahead-coordinate-pair-with-road (first (filter #(:road @(place %)) ahead-coordinate-pairs-in-board))]
      (Thread/sleep car-sleep-ms)
      (if ahead-coordinate-pair-with-crossing
        (let [ahead (place ahead-coordinate-pair-with-crossing)
              ahead-info @ahead]
          (if (:crossing ahead-info)
            (do
              (dosync
                (alter ahead assoc :waiting-queue (conj (:waiting-queue ahead-info) coordinates)))
              (loop [ahead-info @ahead]
                (let [waiting-queue (:waiting-queue ahead-info)]
                  (if (= (first waiting-queue) coordinates)
                    (dosync
                      (move p ahead)
                      (behave car [(:x ahead-info) (:y ahead-info)])
                      (alter ahead assoc :waiting-queue (vec (rest waiting-queue)))
                      car)
                    (recur @ahead)))))
            (if (:tr-light ahead-info)
              car)))
        (if ahead-coordinate-pair-with-road
          (let [ahead (place ahead-coordinate-pair-with-road)
                ahead-info @ahead]
            (dosync
              (move p ahead)
              (send-off *agent* behave [(:x ahead-info) (:y ahead-info)]))
            car))))))

(defn build-road-on-place [x y]
  (let [p (place [x y])]
    (if (:road @p)
      (if (zero? (rand-int 2))
        (alter p assoc :crossing {:waiting-queue []})
        (alter p assoc :tr-light 1))
      (alter p assoc :road 1))))

(defn setup-city []
  (dosync
    (doseq [x-fixed? [true false]]
      (loop [last-fixed-dim 1] ;roads cannot be borders of space, beacause of car starting
        (if (zero? (rand-int 2))
          (let [fixed-dim (+ min-road-length last-fixed-dim (rand-int (inc max-road-length)))
                tilt-deviation (rand-int (min max-road-tilt-deviation (- dim fixed-dim min-road-tilt)))]
            (doseq [free-dim (range dim)]
              (let [tilted-fixed-dim (+ fixed-dim (quot (* (+ tilt-deviation min-road-tilt) free-dim) dim))]
                (if x-fixed?
                  (build-road-on-place tilted-fixed-dim free-dim)
                  (build-road-on-place free-dim tilted-fixed-dim))))
            (when (< (+ min-road-length fixed-dim tilt-deviation min-road-tilt max-road-length) dim--)
              ;roads cannot be borders of space, beacause of car startin
              (recur (+ fixed-dim tilt-deviation min-road-tilt))))
          (let [fixed-dim (+ min-road-length last-fixed-dim (rand-int (inc max-road-length)))]
            (doseq [free-dim (range dim)]
              (if x-fixed?
                (build-road-on-place fixed-dim free-dim)
                (build-road-on-place free-dim fixed-dim)))
            (when (< (+ min-road-length fixed-dim max-road-length) dim--)
              ;roads cannot be borders of space, beacause of car startin
              (recur fixed-dim))))))))

(defn setup-car [[x y :as coordinates] direction]
  (let [car (agent {:img 0
                  :road 0
                  :dir direction
                  :forced-speed 0 
                  :speed-pref 0
                  :left-or-right :left})]
    (dosync
      (alter (place coordinates) assoc :car car))
    {:x x :y y :car car}))

(defn setup-cars []
  (remove nil? (for [pos (range dim)
                     [dir & coordinates] [[0 pos 0] [2 0 pos] [6 dim-- pos] [4 pos dim--]]]
                 (when (:road @(place coordinates)) (setup-car coordinates dir)))))

(use 'city-traffic.ui)

(defn -main []
  (setup-city)
  (def cars (setup-cars))
  (send-off animator animation)
  (dorun (map #(send-off (:car %) behave [(:x %) (:y %)]) cars)))
