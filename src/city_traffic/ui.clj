(ns city-traffic.ui
  (:use [city-traffic.core]))

(import
 '(java.awt Color Graphics Dimension)
 '(java.awt.image BufferedImage)
 '(javax.swing JPanel JFrame))

;pixels per world cell
(def scale 10)

(defn fill-cell [^Graphics g x y c]
  (doto g
    (.setColor c)
    (.fillRect (* x scale) (* y scale) scale scale)))

(defn render-building [building ^Graphics g x y]
  (doto g
    (.setColor (new Color 255 150 0))
    (.fillRect (* x scale) (* y scale) scale scale)))

(defn render-crossing [crossing ^Graphics g x y]
  (doto g
    (.setColor (new Color 255 0 0))
    (.fillRect (* x scale) (* y scale) scale scale)))

(defn render-tr-light [tr-light ^Graphics g x y]
  (doto g
    (.setColor (new Color 255 0 255))
    (.fillRect (* x scale) (* y scale) scale scale)))

(defn render-road [road ^Graphics g x y]
  (doto g
    (.setColor (new Color 0 255 0))
    (.fillRect (* x scale) (* y scale) scale scale)))

(defn render-car [car ^Graphics g x y]
  (doto g
    (.setColor (new Color 0 0 255))
    (.fillRect (* x scale) (* y scale) scale scale)))

(defn render-place [g p x y]
  (when (:building p) (render-building (:building p) g x y))
  (when (:road p)     (render-road (:road p) g x y))
  (when (:tr-light p) (render-tr-light (:tr-light p) g x y))
  (when (:crossing p) (render-crossing (:crossing p) g x y))
  (when (:car p)      (render-car (:car p) g x y)))

(defn render [^Graphics g]
  (let [v (dosync (vec (for [x (range dim) y (range dim)]
                         @(place [x y]))))
        img (BufferedImage. (* scale dim) (* scale dim)
                            BufferedImage/TYPE_INT_ARGB)
        bg (.getGraphics img)]
    (doto bg
      (.setColor Color/WHITE)
      (.fillRect 0 0 (.getWidth img) (.getHeight img)))
    (doseq [x (range dim) y (range dim)]
      (render-place bg (v (+ (* x dim) y)) x y))
    (.drawImage g img 0 0 nil)
    (.dispose bg)))

(def ^JPanel panel (doto (proxy [JPanel] []
                           (paint [g] (render g)))
                     (.setPreferredSize (Dimension.
                                         (* scale dim)
                                         (* scale dim)))))

(def frame (doto (JFrame.) (.add panel) .pack .show))

(def animator (agent nil))

(defn animation [x]
  (when running
    (send-off *agent* #'animation))
  (.repaint panel)
  (Thread/sleep animation-sleep-ms)
  nil) 
