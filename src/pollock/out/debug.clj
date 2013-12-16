(ns pollock.out.debug
  [:require [quil.core :as q] 
            [clojure.pprint :as pp]
            [pollock.gen :as gen]
            [pollock.draw :as draw]
            [pollock.util.mouse-camera :as camera]
            [pollock.util.axis :as axis]])


(def strokes (atom []))
(def splats (atom []))

(defn atom-set! [atom val]
  (swap! atom (fn [old] val)))

(defn gen-paths [options]
  (let [[out-strokes out-splats] (gen/artwork options)]
    (atom-set! strokes
               out-strokes)
    (atom-set! splats
               out-splats)
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

(def draw-strokes? (atom true))
(def draw-splats? (atom true))
(def draw-velocities? (atom false))

(defn toggle! [bool-atom]
  (swap! bool-atom #(if % false true)))

(defn key-pressed [options]
  (case (q/key-code)
    65 (toggle! draw-velocities?)      ;; a
    83 (toggle! draw-strokes?)         ;; s
    68 (toggle! draw-splats?)          ;; d
    71 (gen-paths options)             ;; g
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
  (if @draw-splats? (draw/splatter @splats))
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

(defn start [options]
  
  (println "Generating" (:num-strokes options) "strokes...")
  (gen-paths options)
  (println "Done.")
  
  (show-window options))

