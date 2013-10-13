(ns jackson-quil.util
  [:import [java.util Random]]
  [:refer-clojure :exclude [rand rand-int]])


(defn map-2 [f coll]
  (when-let [s (seq coll)]
    (let [s1 (first s)
          s2 (second s)]
      (if (not (nil? s2))
        (cons (f (first s) (second s)) (map-2 f (rest s)))))))

(defn bound [in in-max out-max]
  (int (* (/ in in-max) out-max)))

(defn vec-mult-const [[i j k] c]
  [(* i c) (* j c) (* k c)])

(defn vec-add [[i1 j1 k1] [i2 j2 k2]]
  [(+ i1 i2) (+ j1 j2) (+ k1 k2)])

(defn vec-sub [[i1 j1 k1] [i2 j2 k2]]
  [(- i1 i2) (- j1 j2) (- k1 k2)])

(defn vec-div-const [[i j k] c]
  [(/ i c) (/ j c) (/ k c)])

(defn vec-abs [vec]
  (Math/sqrt (reduce + (map #(* % %) vec))))

(defn vec-unit [vec]
  (vec-div-const vec (vec-abs vec)))

(defn dot-product [v1 v2]
  (reduce + (map * v1 v2)))

(defn vec-reflect [vec]
  (let [normal [0 1 0]
        vec-dot-normal (dot-product vec normal)
        const (* 2 vec-dot-normal)
        reflection-vec (vec-mult-const normal const)]
    (vec-sub vec reflection-vec)))


(def random-generator (new Random))

(defn set-seed [seed] (.setSeed random-generator (long seed)))

(defn rand
  ([] (.nextFloat random-generator))
  ([n] (* n (rand))))

(defn rand-int [n] (int (rand n)))

(defn random-between [lower upper]
  (let [diff (- upper lower)]
    (+ (rand-int diff) lower)))

(defn random-vector [lower upper]
  [(random-between lower upper)
   (random-between lower upper)
   (random-between lower upper)])

(defn random-unit-vector []
  (let [asimuth (* (rand) 2 Math/PI)
        z (- (rand 2) 1)
        a (Math/sqrt (- 1 (* z z)))
        x (* (Math/cos asimuth) a)
        y (* (Math/sin asimuth) a)]
    [x y z]))

(defn random-path [point step-vector bounds]
  (cons
    point
    (lazy-seq
      (random-path
        (vec-add (vec-add point step-vector) (random-vector (- 0 bounds) bounds))
        step-vector bounds))))
