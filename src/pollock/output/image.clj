(ns pollock.output.image
  [:require [quil.core :as q]
            [pollock.util :as util]
            [pollock.generate :as gen]])


(defn draw-sub-stroke [[x1 y1 z1 i1 j1 k1 p1] [x2 y2 z2 i2 j2 k2 p2]]
  (q/stroke-weight p1)
  (q/line x1 z1 x2 z2))

(defn draw-stroke [stroke color]
  (apply q/stroke color)
  (q/stroke-cap :round)
  (doall (util/map-2 draw-sub-stroke stroke)))

(defn draw-splat [[x y z i j k p] color]
  (apply q/stroke color)
  (apply q/fill color)
  (q/ellipse x z p p))

(defn draw-strokes [{:keys [stroke splatter color]}]
  (draw-stroke stroke color)
  (doall (map #(draw-splat % color) splatter)))

(defn assemble-canvas [width height background-path]
  (let [background-image (q/create-image width height 1)
        canvas-image (q/load-image background-path)
        canvas-width (.width canvas-image)
        canvas-height (.height canvas-image)
        num-x (Math/ceil (/ width canvas-width))
        num-y (Math/ceil (/ height canvas-height))]
    (doall (map (fn [x-index]
                  (doall (map (fn [y-index]
                                (let [dx1 (* x-index canvas-width)
                                      dy1 (* y-index canvas-height)
                                      x-edge (min (+ dx1 canvas-width) width)
                                      y-edge (min (+ dy1 canvas-height) height)
                                      copy-width (- x-edge dx1)
                                      copy-height (- y-edge dy1)]
                                  (.copy background-image canvas-image
                                         0 0 copy-width copy-height
                                         dx1 dy1 copy-width copy-height)))
                              (range num-y))))
                (range num-x)))
    background-image))


(defn draw [output-path options]
  (let [strokes (gen/artwork options)
        background (-> options :colors :background)]
    (if (string? background)
      (q/background-image (assemble-canvas (-> options :dimensions :width)
                                           (-> options :dimensions :depth)
                                           background))
      (apply q/background background))
    (doall (map draw-strokes strokes))
    (q/save output-path)
    (System/exit 0)))


(defn start [output-path options]
  (q/sketch
   :setup #(q/smooth)
   :draw (partial draw output-path options)
   :size [(-> options :dimensions :width)
          (-> options :dimensions :depth)]
   :target :none))
