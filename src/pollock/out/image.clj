(ns pollock.out.image
  [:require [quil.core :as q]
            [pollock.gen :as gen]
            [pollock.util :as util]])




(defn generate-strokes [num-strokes options]
  (println "Generating" num-strokes "strokes...")
  (let [dimensions (:dimensions options)
        max-stroke-length (/ (max (:width dimensions) (:depth dimensions)) 4)
        min-stroke-length (/ (min (:width dimensions) (:depth dimensions)) 4)
        splatter-opt (:splatter options)

        start-points (take num-strokes
                           (gen/start-points (:width dimensions)
                                             (:height dimensions)
                                             (:depth dimensions)))

        air-strokes (map #(gen/add-paint % (:flow-rate options))
                         (map gen/linear-path-velocity
                              (filter gen/path-above-canvas?
                                      (map #(gen/random-path % min-stroke-length
                                                             max-stroke-length)
                                           start-points))))

        strokes (map #(gen/path-projection % (:gravity options))
                     air-strokes)

        min-impact (gen/splatter-min-impact strokes (:mass-per-unit options)
                                            (:percentile splatter-opt))

        splatter (gen/splatter strokes (:mass-per-unit options)
                               min-impact (:likelihood splatter-opt)
                               (:velocity-dampening splatter-opt)
                               (:paint-dampening splatter-opt)
                               (:gravity options))]
    (println "Done.")
    [strokes splatter]))


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


(defn draw [num-strokes output-path options]
  (let [[strokes splatter] (generate-strokes num-strokes options)]
    (println "Drawing...")
    (apply q/background (-> options :colors :background))
    (doall (map draw-stroke strokes))
    (doall (map draw-splat splatter))
    (q/save output-path)
    (println "Done")
    (System/exit 0)))


(defn start [num-strokes output-path options]
  (q/sketch
   :setup #(q/smooth)
   :draw (partial draw num-strokes output-path options)
   :size [(-> options :dimensions :width)
          (-> options :dimensions :depth)]
   :target :none))
