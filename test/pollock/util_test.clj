(ns pollock.util-test
  (:require [clojure.test :refer :all]
            [pollock.util :as util]))

(deftest map-2-test
  (testing "Adding"
    (is (= [3 5] (util/map-2 + [1 2 3])))))

(deftest vec-mult-const-test
  (testing "Vector constant multiplication"
    (is (= [3 3 3] (util/vec-mult-const [1 1 1] 3)))))

(deftest vec-add-test
  (testing "Vector vector addition"
    (is (= [3 3 3] (util/vec-add [1 1 1] [2 2 2])))))

(deftest vec-sub-test
  (testing "Vector vector subtraction"
    (is (= [2 2 2] (util/vec-sub [3 3 3] [1 1 1])))))

(deftest vec-div-const-test
  (testing "Vector constant divison"
    (is (= [6 6 6] (util/vec-div-const [30 30 30] 5)))))

(deftest vec-abs
  (testing "Vector absolute value"
    (is (= 5.0 (util/vec-abs [3 4])))))

(deftest dot-product
  (testing "Dot product of two vectors"
    (is (= -42 (util/dot-product [2 -3 7] [-4 2 -4])))))

(deftest vec-refect
  (testing "Simple reflection"
    (is (= [1 1 0] (util/vec-reflect [1 -1 0])))))

