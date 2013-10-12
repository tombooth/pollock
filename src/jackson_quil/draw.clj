(ns jackson-quil.draw
  [:require [quil.core :as q]
            [clojure.pprint :as pp]
            [jackson-quil.util :as util]])


(defn velocity [[x y z i j k p]]
  (q/stroke (q/color 0 255 0))
  (q/begin-shape)
  (q/vertex x y z)
  (q/vertex (+ x i) y z)
  (q/end-shape)
  (q/stroke (q/color 0 0 255))
  (q/begin-shape)
  (q/vertex x y z)
  (q/vertex x (+ y j) z)
  (q/end-shape)
  (q/stroke (q/color 255 0 0))
  (q/begin-shape)
  (q/vertex x y z)
  (q/vertex x y (+ z k))
  (q/end-shape))

(defn velocities [path]
  (q/push-style)
  (doall (map velocity path))
  (q/pop-style))

(defn path-slice [[x1 y1 z1 i1 j1 k1 p1] [x2 y2 z2 i2 j2 k2 p2]]
  (q/stroke-weight p1)
  (q/vertex x1 y1 z1)
  (q/vertex x2 y2 z2))

(defn path [path]
  (q/push-style)
  (q/stroke-cap :round)
  (q/begin-shape :lines)
  (doall (util/map-2 path-slice path))
  (q/end-shape)
  (q/pop-style))

(defn splatter [points]
  (q/begin-shape :points)
  (q/stroke (q/color 255 0 0))
  (doall(map
         (fn [[x y z i j k p]]
           (q/stroke-weight p)
           (q/vertex x y z))
         points))
  (q/end-shape))



