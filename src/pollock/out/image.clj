(ns pollock.out.image
  [:require [quil.core :as q]
            [pollock.gen :as gen]
            [pollock.util :as util]])



(defn draw-sub-stroke [[x1 y1 z1 i1 j1 k1 p1] [x2 y2 z2 i2 j2 k2 p2]]
  (q/stroke-weight p1)
  (q/line x1 z1 x2 z2))

(defn draw-stroke [stroke]
  (q/stroke 0 0 0)
  (q/stroke-cap :round)
  (doall (util/map-2 draw-sub-stroke stroke)))


(defn draw-splat [[x y z i j k p]]
  (q/stroke 255 0 0)
  (q/fill 255 0 0)
  (q/ellipse x z p p))


(defn assemble-canvas [width height]
  (let [background-image (q/create-image width height 1)
        canvas-image (q/load-image "resources/canvas.jpg")
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
  (let [[strokes splatter] (gen/artwork options)]
    (println "Drawing...")
    (if (string? (-> options :colors :background))
      (q/background-image (assemble-canvas (-> options :dimensions :width)
                                           (-> options :dimensions :depth)))
      (apply q/background (-> options :colors :background)))
    (doall (map draw-stroke strokes))
    (doall (map draw-splat splatter))
    (q/save output-path)
    (println "Done")
    (System/exit 0)))


(defn start [output-path options]
  (q/sketch
   :setup #(q/smooth)
   :draw (partial draw output-path options)
   :size [(-> options :dimensions :width)
          (-> options :dimensions :depth)]
   :target :none))
