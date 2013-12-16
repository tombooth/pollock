(ns pollock.generate-test
  (:require [clojure.test :refer :all]
            [pollock.entry :as entry]
            [pollock.generate :refer :all]))

(def gravity [0 -9.8 0])

(defn- straight-line-seq [x]
  (cons [x 10 0.0 0 0 0]
        (lazy-seq (straight-line-seq (+ x 1.0)))))

(defn- y-to-zero [[x y z i j k]]
  [x 0 z i j k])

(defn- position [[x y z i j k]]
  [x y z])

(defn- positions [points]
  (map position points))

(defn- every-point-the-same? [expected-line projected-line]
  (let [zipped-lines (map #(vector (float %1) (float %2))
                            (flatten expected-line) (flatten projected-line))]
    (every? #(apply = %1) zipped-lines)))

(deftest simple-projection-test
  
  (testing "No velocity and gravity results in duplicate"
    (let [line (take 10 (straight-line-seq 1))
          projected-line (positions
                          (path-projection line gravity))
          expected-line (positions (map y-to-zero line))]
      (is (every-point-the-same? expected-line projected-line))))
  
  (testing "Velocity in i plane"
    (let [line [[0  10 0 1  0 0]
                [5  10 0 5  0 0]
                [10 10 0 10 0 0]]
          expected-line [[10/7  0 0]
                         [85/7  0 0]
                         [170/7 0 0]]
          projected-line (positions
                          (path-projection line gravity))]
      (is (every-point-the-same? expected-line projected-line)))))


(deftest splatter-projection-test
  (testing "That it doesn't just pick the start point"
    (let [point [0 0 0 1 5 0 1]
          expected-point [10.0 0 0.0 1.0 -5.0 0.0 1]
          projected-point (point-projection point [0 -1 0])]
      (is (= expected-point projected-point)))))

(deftest splatter-test
  (testing "Paths to points"
    (is (= [[1 2 3] [4 5 6]] (paths-to-points [[[1 2 3]] [[4 5 6]]]))))
  
  (testing "Does it splatter pred"
    (is (does-impact-splatter? [3 4 5] 5 30 1.0)))

  (testing "End to end splatter"
    (let [path [[0 0 0 1 -5 0 1]]
          expected-splatter-points [[10.0 0 0.0 1.0 -5.0 0.0 1.0]]
          splatter-points (splatter path 1.0 0 1.0 1.0 1.0 [0 -1 0])]
      (is (= splatter-points expected-splatter-points)))))


(deftest paint-test
  (testing "Paint added according to flow rate"
    (let [path [[0] [1] [2] [3]]
          expected-with-paint [[0 10]
                               [1 5]
                               [2 10/3]
                               [3 5/2]]
          flow-rate 1
          initial 10
          with-paint (add-paint path flow-rate initial)]
    (is (= with-paint expected-with-paint)))))

(deftest artwork-test
  (testing "Simple artwork gen test"
    (let [strokes (artwork entry/default-config)]
      (is (= (count strokes) (:num-strokes entry/default-config)))
      (is (every? #(and (:stroke %) (:splatter %)) strokes)))))
