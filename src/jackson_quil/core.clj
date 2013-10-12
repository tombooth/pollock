(ns jackson-quil.core
  [:require [quil.core :as q]
            [clojure.pprint :as pp]
            [jackson-quil.gen :as gen]
            [jackson-quil.draw :as draw]
            [jackson-quil.util.mouse-camera :as camera]
            [jackson-quil.util.axis :as axis]])

(def desired-dpi 300)
(def actual-dpi 72)

(def desired-dpcm (quot desired-dpi 2.54))
(def actual-dpcm (quot actual-dpi 2.54))
(def dpcm-scaling (/ desired-dpi actual-dpi))

(def width-cm 10)
(def depth-cm 10)
(def height-cm 5)

(def width (* (* width-cm actual-dpcm) dpcm-scaling))
(def depth (* (* depth-cm actual-dpcm) dpcm-scaling))
(def height (* (* height-cm actual-dpcm) dpcm-scaling))
(def max-side (max width height))

;; gravity is -980cms-2 and we need to convert this to pixels
(def gravity [0 (* -980 actual-dpcm) 0])

;; splatter is setup so that any impact points with a speed higher
;; than the percentile set here will generate splatter
(def splatter-percentile 90)

;; this is what the reflected splatted velocity vector will be
;; multiplied with during splatter calculation. it should be in
;; between 0.0 and 1.0. it is 3.0 at the moment because gravity is
;; super strong and i'm not sure how i want to tweak all params.
(def splatter-dampening 3.0)

;; this is the 'flow rate' of the paint. this translates as how much
;; paint each point along the stroke recieves as function of the
;; initial amount. this is initial * (1 / [1 + (point_num * flow-rate)])
(def flow-rate 0.08)

(def start-points (atom []))
(def strokes (atom []))
(def streaks (atom []))
(def splatter (atom []))

(defn atom-set! [atom val]
  (swap! atom (fn [old] val)))

(defn gen-paths [n]
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
  (let [cut-off (gen/splatter-cut-off @streaks splatter-percentile)]
    (atom-set! splatter
               (doall (gen/splatter @streaks cut-off splatter-dampening gravity))))
  nil)

(defn align-camera []
  (comment (let [x-mid (int (/ width 2))
        z-mid (int (/ depth 2))]
    (camera/set-camera! x-mid 580 241 x-mid 0 z-mid 0 -1 0)))
  (camera/set-camera! 583 1122 581 583 0 583 0 -1 0))

(defn draw-layout []
  (q/push-style)
  (axis/draw)
  (q/stroke 0)
  (q/no-fill)
  (q/begin-shape :quads)
    (q/vertex 0 0 0)
    (q/vertex 0 0 depth)
    (q/vertex width 0 depth)
    (q/vertex width 0 0)
  (q/end-shape)
  (q/pop-style))

(def draw-strokes? (atom false))
(def draw-velocities? (atom false))

(defn toggle! [bool-atom]
  (swap! bool-atom #(if % false true)))

(defn key-pressed []
  (println "Code: " (q/key-code))
  (case (q/key-code)
    83 (toggle! draw-strokes?)         ;; s
    68 (toggle! draw-velocities?)      ;; d
    71 (gen-paths 10)                  ;; g
    81 (align-camera)                  ;; q
    87 (camera/set-camera! 577 757 -1019 583 400 583 0 -1 0) ;; w
    (println "Key-code: " (q/key-code) " pressed, unknown.")))

(defn draw []
  (q/background 255)
  (camera/move-camera)
  (draw-layout)
  (q/push-style)
  (if @draw-strokes? (doall (map draw/path @strokes)))
  (if @draw-velocities? (doall (map draw/velocities @strokes)))
  (doall (map draw/path @streaks))
  (doall (draw/splatter @splatter))
  (q/pop-style))

(defn setup []
  (q/smooth)
  (q/background 255)
  (align-camera))

(defn show-window []
  (q/sketch
    :title "Jackson Phonelock Playground"
    :setup setup
    :draw draw
    :mouse-pressed camera/mouse-pressed
    :mouse-dragged camera/mouse-dragged
    :key-pressed key-pressed
    :size [640 480]
    :renderer :p3d
    :target :frame))

(defn -main []
  (gen-paths 10)
  (show-window))
