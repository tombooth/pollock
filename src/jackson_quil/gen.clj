(ns jackson-quil.gen
  [:require [clojure.pprint :as pp]
            [jackson-quil.de-casteljau :as dc]
            [jackson-quil.util :as util]])


;; Projection

(defn project-seq
  "Takes a vector containing the position of a point along side it's current
  velocity, a vector defining the environments intrinsic acceleration (gravity)
  and the time per step in secconds. It returns a lazy-sequence progressing
  the point through time at t second intervals."
  [point env-acceleration time]
  (let [[position velocity paint] (partition-all 3 point)
        velocity-delta (util/vec-mult-const env-acceleration time)
        new-velocity (util/vec-add velocity velocity-delta)
        new-position (util/vec-add position (util/vec-mult-const velocity time))
        new-point (concat new-position new-velocity paint)]
    (cons
      new-point
      (lazy-seq (project-seq new-point env-acceleration time)))))

(defn above-canvas? [[x y & _]] (> y 0))

(defn path-above-canvas? [path] (every? above-canvas? path))

(defn point-projections [point gravity]
  (take-while above-canvas? (project-seq point gravity 0.01)))

(defn path-projections [path gravity]
  (map #(point-projections % gravity) path))

(defn perfect-point-projection [[x0 y0 z0 i0 j0 k0 p] [ai aj ak]]
  (let [time-discrim (Math/sqrt (- (* 4 j0 j0) (* 8 aj y0)))
        t-1 (/ (- time-discrim (* 2 j0)) (* 2 aj))
        t-2 (/ (- (- 0 (* 2 j0)) time-discrim) (* 2 aj))
        x-diff (+ (* i0 t-2) (/ (* ai t-2 t-2) 2))
        z-diff (+ (* k0 t-2) (/ (* ak t-2 t-2) 2))
        x (+ x0 x-diff)
        z (+ z0 z-diff)
        i (+ i0 (* ai t-2))
        j (+ j0 (* aj t-2))
        k (+ k0 (* ak t-2))]
    [x 0 z i j k p]))

(defn perfect-path-projection [path gravity]
  (map #(perfect-point-projection % gravity) path))

(defn extract-path-projection [path-projection gravity]
  (map #(perfect-point-projection (last %1) gravity) path-projection))



;; Path generation

(defn start-points [width height depth]
  (cons
    [(rand width) (rand height) (rand depth)]
    (lazy-seq (start-points width height depth))))

(defn dribble-path [point min-distance max-distance]
  (let [distance (util/random-between min-distance max-distance)
        steps (util/random-between 3 15)
        direction (util/random-unit-vector)
        step-vector (util/vec-mult-const direction (/ distance steps))
        end-point (util/vec-add point (util/vec-mult-const step-vector steps))
        random-points (take steps (util/random-path point step-vector 50))
        anchor-points (conj (vec random-points) end-point)]
    (dc/calc anchor-points 0.01)))

(defn flick-path [point min-distance max-distance]
  (let [distance (util/random-between min-distance max-distance)
        steps 2
        direction (util/random-unit-vector)
        step-vector (util/vec-mult-const direction (/ distance steps))
        end-point (util/vec-add point (util/vec-mult-const step-vector steps))
        random-points (take steps (util/random-path point step-vector distance))
        anchor-points (conj (vec random-points) end-point)]
    (dc/calc anchor-points 0.01)))

(defn random-path [point min-distance max-distance]
  (let [path-fn (if (> 0.5 (rand)) dribble-path flick-path)]
      (path-fn point min-distance max-distance)))


;; Velocity generation

(defn linear-point-velocity [time start-point end-point]
  (let [distance-vector (util/vec-sub end-point start-point)
        [i j k] (util/vec-div-const distance-vector time)]
    (conj (vec start-point) i j k)))

(defn linear-path-velocity [path]
  (let [total-time (util/random-between 5 10)
        step-time (/ total-time (count path))]
    (util/map-2 #(linear-point-velocity step-time %1 %2) path)))


;; Paint generation
;; ----------------
;;
;; We need to give every point an amount of paint. Every path should
;; have a random amount of point allocated to it (within some bounds)
;; and then each point along the path will get allocated a decreasing
;; amount of paint from that total.

(defn- unbound-range [start step]
  (cons start (lazy-seq (unbound-range (+ start step) step))))

(defn add-paint
  
  ([path flow-rate]
     (add-paint path flow-rate (util/random-between 1 20)))
  
  ([path flow-rate initial-amount]
     (map #(conj %1 %2)
          path
          (map #(* initial-amount (/ 1 %))
               (unbound-range 1 flow-rate)))))


;; Splatter generation
;; -------------------
;;
;; The below is all under the assumption that velocities are directed
;; into the canvas.
;;
;; To do splatter we want to break it into a few parts:
;;   - See if the impact was hard enough to cause splatter
;;   - Refect the impact vector and veloctiy (with dampening)
;;   - Hopefully reuse the projection function to find out where the
;;     splatter will land. Generating a new impact vector and velocity

(defn does-impact-splatter? [velocity-vector cut-off]
  (let [magnitude (util/vec-abs velocity-vector)]
    (> magnitude cut-off)))


(defn splatter-point [point cut-off damp-const gravity]
  (let [[position velocity paint] (partition-all 3 point)]
    (if (does-impact-splatter? velocity cut-off)
      (let [reflect-velocity (util/vec-reflect velocity)
            dampend-veloctiy (util/vec-mult-const reflect-velocity damp-const)
            projected (perfect-point-projection
                       (concat position dampend-veloctiy [1])    ;; paint amount hardcoded
                       gravity)]
        projected)
      nil)))

(defn paths-to-points [paths]
  (apply concat paths))

(defn splatter [paths cut-off damp-const gravity]
  (filter #(not (nil? %))
          (map #(splatter-point % cut-off damp-const gravity)
               (paths-to-points paths))))

(defn splatter-cut-off [paths percentile]
  (let [magnitudes (map
                    (fn [[x y z i j k p]] (util/vec-abs [i j k]))
                    (paths-to-points paths))
        sorted-magnitudes (sort magnitudes)
        index (Math/ceil (* (/ percentile 100) (count magnitudes)))]
    (nth sorted-magnitudes index)))


