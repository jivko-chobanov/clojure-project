(ns city-traffic.core)

(def running true)
(def dim 50)
(def dim-- (dec dim))
(def car-length 1)
(def min-road-length (* 4 car-length))
(def max-road-length-deviation (* 3 car-length))
(def min-road-tilt (* 3 car-length))
(def max-road-tilt-deviation (* 9 car-length))

(def animation-sleep-ms 50)
(def car-sleep-ms 100)
(def min-tr-light-sleep 800)
(def max-tr-light-sleep 3000)
(def tr-light-sleep-deviation (- max-tr-light-sleep min-tr-light-sleep))

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

(declare behave-on-crossing)

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
                                                           (or (when-let [crossing-info (:crossing ahead-info)]
                                                                 (some #{(:dir car)} (:line-dirs crossing-info)))
                                                               (when-let [tr-info (:tr-light-crossing ahead-info)]
                                                                 (some #{(:dir car)} (:line-dirs tr-info))))))
                                                       ahead-coordinate-pairs-in-board))
          ahead-coordinate-pair-with-road (first (filter 
                                                   (fn [ahead-coordinates]
                                                     (let [ahead-info @(place ahead-coordinates)]
                                                       (when-let [road-info (:road ahead-info)]
                                                         (= (:line-dir road-info) (:dir car)))))
                                                   ahead-coordinate-pairs-in-board))]
      (Thread/sleep car-sleep-ms)
      (if ahead-coordinate-pair-with-crossing
        (behave-on-crossing ahead-coordinate-pair-with-crossing car coordinates)
        (if ahead-coordinate-pair-with-road
          (let [ahead (place ahead-coordinate-pair-with-road)
                ahead-info @ahead]
            (dosync
              (move p ahead)
              (send-off *agent* behave [(:x ahead-info) (:y ahead-info)]))
            car))))))

(defn behave-on-crossing [ahead-coordinate-pair-with-crossing car coordinates]
  (let [ahead (place ahead-coordinate-pair-with-crossing)
        ahead-info @ahead
        p (place coordinates)]
    (if (:crossing ahead-info)
      (do
        (dosync
          (alter ahead assoc-in [:crossing :waiting-queue]
                 (conj (get-in ahead-info [:crossing :waiting-queue]) coordinates)))
        (loop [ahead-info @ahead]
          (let [waiting-queue (get-in ahead-info [:crossing :waiting-queue])]
            (if (= (first waiting-queue) coordinates)
              (dosync
                (move p ahead)
                (behave car [(:x ahead-info) (:y ahead-info)])
                (alter ahead assoc-in [:crossing :waiting-queue] (vec (rest waiting-queue)))
                car)
              (recur @ahead)))))
      (if (:tr-light-crossing ahead-info)
        (loop [ahead-info @ahead]
          (if-let [trl (:tr-light ahead-info)] (if-let [err (agent-error trl)] (prn ahead-info err)))
          @(get-in ahead-info [:tr-light-crossing :promise])
          (dosync
            (move p ahead)
            (behave car [(:x ahead-info) (:y ahead-info)])
            car))))))

(defn tr-light-behave [tr-light]
  (dosync
    (doseq [coordinates (:coordinate-pairs tr-light)
            tr-light-crossing-info (:tr-light-crossing @(place coordinates))]
      (if ((set (:green-dirs tr-light)) (:line-dirs tr-light-crossing-info))
        (deliver (:promise tr-light-crossing-info) true)
        (if (realized? (:promise tr-light-crossing-info))
          (alter (place coordinates) assoc-in [:tr-light-crossing :promise] (promise))))))
  (Thread/sleep (:sleep tr-light))
  (send-off *agent* tr-light-behave)
  (assoc tr-light :green-dirs (map #(rem (+ % 2) 8) (:green-dirs tr-light))))

(defn road-builder [places-with-dirs rand-bool]
  (doseq [[p dir dir2] places-with-dirs]
    (let [place-info @p]
      (dosync
        (if (:road place-info)
          (if rand-bool
            (alter p assoc :crossing {:line-dirs [dir dir2] :waiting-queue []})
            (let [spread [-1 0 1]
                  coordinates [(:x place-info) (:y place-info)]
                  coords-around-in (for [x (map #(+ (:x place-info) %) spread)
                                         y (map #(+ (:y place-info) %) spread)
                                         :when (and (>= x 0) (< x dim) (>= y 0) (< y dim))]
                                     [x y])]
              (alter p assoc :tr-light-crossing {:line-dirs [dir dir2] :promise (promise)})
              (if-let [tr-light-coords (first (filter #(:tr-light @(place %)) coords-around-in))]
                (let [tr-light (:tr-light @(place tr-light-coords))
                      new-coordinate-pairs (conj (:coordinate-pairs @tr-light) coordinates)]
                  (send-off tr-light assoc :coordinate-pairs new-coordinate-pairs))
                (let [tr-light (agent {:green-dirs [dir2 (rem (+ dir2 4) 8)]
                                                   :coordinate-pairs [coordinates]
                                                   :sleep (+ min-tr-light-sleep
                                                             (rand-int tr-light-sleep-deviation))})]
                  (alter p assoc :tr-light tr-light)))))
          (alter p assoc :road {:line-dir dir}))))))

(defn build-road-on-place [x y vertical? rand-bool]
  (let [coordinates-with-dirs (if vertical? 
                                [[x y 0 6] [(inc x) y 4 6] [x (inc y) 0 2] [(inc x) (inc y) 4 2]]
                                [[x y 6 4] [x (inc y) 2 4] [(inc x) y 6 0] [(inc x) (inc y) 2 0]])]
    (road-builder (map #(vector (place (take 2 %)) (nth % 2) (nth % 3)) coordinates-with-dirs) rand-bool)))

(defn setup-city []
  (doseq [x-fixed? [true false]]
    (loop [last-fixed-dim 1 rand-bool (zero? (rand-int 2)) old true]
      (if (zero? (rand-int 2))
        (let [fixed-dim (+ min-road-length last-fixed-dim (rand-int (inc max-road-length-deviation)))
              tilt-deviation (rand-int (min max-road-tilt-deviation (- dim fixed-dim min-road-tilt)))]
          (doseq [free-dim (range 0 dim 2)]
            (let [tilted-fixed-dim (+ fixed-dim (quot (* (+ tilt-deviation min-road-tilt) free-dim) dim))]
              (if x-fixed?
                (build-road-on-place tilted-fixed-dim free-dim x-fixed? rand-bool)
                (build-road-on-place free-dim tilted-fixed-dim x-fixed? rand-bool))))
          (when (< (+ min-road-length fixed-dim tilt-deviation min-road-tilt max-road-length-deviation) dim--)
            ;roads cannot be borders of space, beacause of car startin
            (recur (+ fixed-dim tilt-deviation min-road-tilt)
                   (if old rand-bool (zero? (rand-int 2)))
                   (not old))))
        (let [fixed-dim (+ min-road-length last-fixed-dim (rand-int (inc max-road-length-deviation)))]
          (doseq [free-dim (range 0 dim 2)]
            (if x-fixed?
              (build-road-on-place fixed-dim free-dim x-fixed? rand-bool)
              (build-road-on-place free-dim fixed-dim x-fixed? rand-bool)))
          (when (< (+ min-road-length fixed-dim max-road-length-deviation) dim--)
            ;roads cannot be borders of space, beacause of car startin
            (recur fixed-dim
                   (if old rand-bool (zero? (rand-int 2)))
                   (not old))))))))

(defn setup-car [[x y :as coordinates] direction]
  (let [car (agent {:img 0
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
                 (when (= (:line-dir (:road @(place coordinates))) dir) (setup-car coordinates dir)))))

(use 'city-traffic.ui)

(defn -main []
  (setup-city)
  (def cars (setup-cars))
  (send-off animator animation)
  (dorun (map #(send-off (:car %) behave [(:x %) (:y %)]) cars)))
