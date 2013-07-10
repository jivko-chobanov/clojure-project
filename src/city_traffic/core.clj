(ns city-traffic.core)

(def running true)
(def dim 20)
(def dim-- (dec dim))
(def car-length 1)
(def min-road-length (* 2 car-length))
(def max-road-length (* 5 car-length))

(def animation-sleep-ms 100)
(def car-sleep-ms 40)

(def world
  (mapv (fn [y]
          (mapv (fn [x] (ref {:x x :y y}))
                (range dim)))
        (range dim)))

(defn place [[x y]]
  (-> world (nth y) (nth x)))

(defn delta-loc [[x y] dir]
  (case dir
    0 [x (inc y)]
    2 [(inc x) y]
    4 [x (dec y)]
    6 [(dec x) y]))

(defn move [old-place new-place]
  (let [car (:car @old-place)]
    (alter old-place dissoc :car)
    (alter new-place assoc :car car)))

(defn behave [car coordinates]
  (let [p (place coordinates)
        ahead-coordinates (delta-loc coordinates (:dir car))]
    (when (every? #(and (>= % 0) (< % dim)) ahead-coordinates)
      (let [ahead (place ahead-coordinates)
            ahead-map @ahead]
        (Thread/sleep car-sleep-ms)
        (when running
          (if (:crossing ahead-map)
            nil
            (if (:tr-light ahead-map)
              nil
              (when (:road ahead-map)
                (dosync
                  (move p ahead)
                  (send-off *agent* behave [(:x ahead-map) (:y ahead-map)]))
                car))))))))

(defn build-road-on-place [x y]
  (let [p (place [x y])]
    (if (:road @p)
      (if (zero? (rand-int 2))
        (alter p assoc :crossing 1)
        (alter p assoc :tr-light 1))
      (alter p assoc :road 1))))

(defn setup-city []
  (dosync
    (doseq [x-fixed? [true false]]
      (loop [last-fixed-dim 1] ;roads cannot be borders of space, beacause of car starting
        (let [fixed-dim (+ min-road-length last-fixed-dim (rand-int (+ 1 max-road-length)))]
          (doseq [free-dim (range dim)]
            (if x-fixed?
              (build-road-on-place fixed-dim free-dim)
              (build-road-on-place free-dim fixed-dim)))
          (when (< (+ min-road-length fixed-dim max-road-length) dim--)
            ;roads cannot be borders of space, beacause of car startin
            (recur fixed-dim)))))))

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
