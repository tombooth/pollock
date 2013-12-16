(ns pollock.out.debug
  [:require [quil.core :as q] 
            [clojure.pprint :as pp]
            [pollock.gen :as gen]
            [pollock.util :as util]
            [pollock.out.mouse-camera :as camera]
            [pollock.out.axis :as axis]])


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

;; Drawing functions

(defn draw-velocity [[x y z i j k p]]
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

(defn draw-velocities [path]
  (q/push-style)
  (doall (map draw-velocity path))
  (q/pop-style))

(defn draw-path-slice [[x1 y1 z1 i1 j1 k1 p1]
                       [x2 y2 z2 i2 j2 k2 p2]]
  (q/stroke-weight p1)
  (q/vertex x1 y1 z1)
  (q/vertex x2 y2 z2))

(defn draw-path [path]
  (q/push-style)
  (q/stroke-cap :round)
  (q/begin-shape :lines)
  (doall (util/map-2 draw-path-slice path))
  (q/end-shape)
  (q/pop-style))

(defn draw-splatter [points]
  (q/begin-shape :points)
  (q/stroke (q/color 255 0 0))
  (doall(map
         (fn [[x y z i j k p]]
           (q/stroke-weight p)
           (q/vertex x y z))
         points))
  (q/end-shape))

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
  (if @draw-strokes? (doall (map draw-path @strokes)))
  (if @draw-velocities? (doall (map draw-velocities @strokes)))
  (if @draw-splats? (draw-splatter @splats))
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

