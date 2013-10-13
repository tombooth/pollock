(ns pollock.util.axis
  [:require [quil.core :as q]])

(defn draw []
  (q/begin-shape :lines)
  (q/stroke (q/color 0 255 0))
  (q/vertex -1000 0 0)
  (q/vertex 1000 0 0)
  (q/end-shape)
  (q/stroke (q/color 0 0 255))
  (q/begin-shape :lines)
  (q/vertex 0 -1000 0)
  (q/vertex 0 1000 0)
  (q/end-shape)
  (q/stroke (q/color 255 0 0))
  (q/begin-shape :lines)
  (q/vertex 0 0 -1000)
  (q/vertex 0 0 1000)
  (q/end-shape))

