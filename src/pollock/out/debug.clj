(ns pollock.out.debug
  [:require [quil.core :as q] 
            [clojure.pprint :as pp]
            [pollock.gen :as gen]
            [pollock.draw :as draw]
            [pollock.util.mouse-camera :as camera]
            [pollock.util.axis :as axis]])


(def start-points (atom []))
(def strokes (atom []))
(def streaks (atom []))
(def splats (atom []))

(defn atom-set! [atom val]
  (swap! atom (fn [old] val)))

(defn gen-paths [n options]
  (let [width (-> options :dimensions :width)
        height (-> options :dimensions :height)
        depth (-> options :dimensions :depth)
        max-side (max width depth)
        flow-rate (:flow-rate options)
        gravity (:gravity options)
        mass-per-unit (:mass-per-unit options)
        splatter (:splatter options)]
    (atom-set! start-points
               (take n (gen/start-points width height depth)))
    (atom-set! strokes
               (doall
                (map #(gen/add-paint % flow-rate)
                     (map gen/linear-path-velocity
                          (filter gen/path-above-canvas?
                                  (map #(gen/random-path % 50 (/ max-side 4))
                                       @start-points))))))
    (atom-set! streaks
               (doall (map #(gen/path-projection % gravity)
                           @strokes)))
    (let [min-impact (gen/splatter-min-impact @streaks mass-per-unit
                                              (:percentile splatter))]
      (atom-set! splats
                 (doall (gen/splatter @streaks mass-per-unit
                                      min-impact (:likelihood splatter)
                                      (:velocity-dampening splatter)
                                      (:paint-dampening splatter)
                                      gravity))))
    nil))

(defn align-camera []
  (camera/set-camera! 583 1122 581 583 0 583 0 -1 0))

(defn draw-layout [options]
  (q/push-style)
  (axis/draw)
  (q/stroke 0)
  (q/no-fill)
  (let [width (-> options :dimensions :width)
        depth (-> options :dimensions :depth)]
    (q/begin-shape :quads)
      (q/vertex 0 0 0)
      (q/vertex 0 0 depth)
      (q/vertex width 0 depth)
      (q/vertex width 0 0)
    (q/end-shape))
  (q/pop-style))

(def draw-strokes? (atom false))
(def draw-velocities? (atom false))

(defn toggle! [bool-atom]
  (swap! bool-atom #(if % false true)))

(defn key-pressed [options]
  (case (q/key-code)
    83 (toggle! draw-strokes?)         ;; s
    68 (toggle! draw-velocities?)      ;; d
    71 (gen-paths 10 options)          ;; g
    81 (align-camera)                  ;; q
    87 (camera/set-camera! 577 757 -1019 583 400 583 0 -1 0) ;; w
    (println "Key-code: " (q/key-code) " pressed, unknown.")))

(defn draw [options]
  (apply q/background (-> options :colors :background))
  (camera/move-camera)
  (draw-layout options)
  (q/push-style)
  (if @draw-strokes? (doall (map draw/path @strokes)))
  (if @draw-velocities? (doall (map draw/velocities @strokes)))
  (doall (map draw/path @streaks))
  (doall (draw/splatter @splats))
  (q/pop-style))

(defn setup [options]
  (q/smooth)
  (align-camera))

(defn show-window [options]
  (q/sketch
    :title "Jackson Phonelock Playground"
    :setup (partial setup options)
    :draw (partial draw options)
    :mouse-pressed camera/mouse-pressed
    :mouse-dragged camera/mouse-dragged
    :key-pressed (partial key-pressed options)
    :size [640 480]
    :renderer :p3d
    :target :frame))

(defn start [num-strokes options]
  
  (println "Generating" num-strokes "strokes...")
  (gen-paths num-strokes options)
  (println "Done.")
  
  (show-window options))

