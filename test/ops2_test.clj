(ns ops2-test
  (:require
   [cycl.ops2 :refer :all]
   [cycl.event :as e]
   [clojure.test :as t :refer [deftest is testing]]
   [matcher-combinators.test]
   [matcher-combinators.matchers :as m]))

(deftest fit-basic-test
  (testing "fit with simple values"
    (is (match? [{:start 0N :length 1/3 :period 1 :params {:init :a}}
                 {:start 1/3 :length 1/3 :period 1 :params {:init :b}}
                 {:start 2/3 :length 1/3 :period 1 :params {:init :c}}]
                (fit :a :b :c))))

  (testing "fit with nested fit"
    (is (match? [{:start 0N :length 1/4 :period 1 :params {:init :a}}
                 {:start 1/4 :length 1/4 :period 1 :params {:init :b}}
                 {:start 1/2 :length 1/2 :period 1 :params {:init :c}}]
                (fit (fit :a :b) :c))))

  (testing "fit with single nested fit"
    (is (match? [{:start 0N :length 1/2 :period 1 :params {:init :a}}
                 {:start 1/2 :length 1/2 :period 1 :params {:init :b}}]
                (fit (fit :a :b)))))

  (testing "fit with nested fit in second position"
    (is (match? [{:start 0N :length 1/2 :period 1 :params {:init :a}}
                 {:start 1/2 :length 1/4 :period 1 :params {:init :b}}
                 {:start 3/4 :length 1/4 :period 1 :params {:init :c}}]
                (fit :a (fit :b :c)))))

  (testing "fit with vector"
    (is (match? [{:start 0N :length 1/2 :period 1 :params {:init :a}}
                 {:start 1/2 :length 1/4 :period 1 :params {:init :b}}
                 {:start 3/4 :length 1/4 :period 1 :params {:init :c}}]
                (fit :a [:b :c]))))

  (testing "fit with nested vector"
    (is (match? [{:start 0N :length 1/2 :period 1 :params {:init :a}}
                 {:start 1/2 :length 1/4 :period 1 :params {:init :b}}
                 {:start 3/4 :length 1/4 :period 1 :params {:init :c}}]
                (fit [:a [:b :c]]))))

  (testing "fit with range"
    (is (match? [{:start 0N :length 1/4 :period 1 :params {:init 0}}
                 {:start 1/4 :length 1/4 :period 1 :params {:init 1}}
                 {:start 1/2 :length 1/4 :period 1 :params {:init 2}}
                 {:start 3/4 :length 1/4 :period 1 :params {:init 3}}]
                (fit (range 4)))))

  (testing "fit with apply range"
    (is (match? [{:start 0N :length 1/4 :period 1 :params {:init 0}}
                 {:start 1/4 :length 1/4 :period 1 :params {:init 1}}
                 {:start 1/2 :length 1/4 :period 1 :params {:init 2}}
                 {:start 3/4 :length 1/4 :period 1 :params {:init 3}}]
                (apply fit (range 4)))))

  (testing "fit with list"
    (is (match? [{:start 0N :length 1/3 :period 1 :params {:init :a}}
                 {:start 1/3 :length 1/3 :period 1 :params {:init :b}}
                 {:start 2/3 :length 1/3 :period 1 :params {:init :c}}]
                (fit (list :a :b :c))))))

(deftest cyc-basic-test
  (testing "cyc with simple values"
    (is (match? [{:start 0 :length 1 :period 3 :params {:init :a}}
                 {:start 1 :length 1 :period 3 :params {:init :b}}
                 {:start 2 :length 1 :period 3 :params {:init :c}}]
                (cyc :a :b :c))))

  (testing "cyc with nested cyc"
    (is (match? [{:start 0 :length 1 :period 4 :params {:init :a}}
                 {:start 1 :length 1 :period 4 :params {:init :b}}
                 {:start 2 :length 1 :period 4 :params {:init :a}}
                 {:start 3 :length 1 :period 4 :params {:init :c}}]
                (cyc :a (cyc :b :c))))))

(deftest fit-cyc-combination-test
  (testing "fit with cyc nested"
    (is (match? [{:start 0N :length 1/2 :period 2 :params {:init :a}}
                 {:start 1/2 :length 1/2 :period 2 :params {:init :b}}
                 {:start 1N :length 1/2 :period 2 :params {:init :a}}
                 {:start 3/2 :length 1/2 :period 2 :params {:init :c}}]
                (fit :a (cyc :b :c)))))

  (testing "cyc with fit nested"
    (is (match? [{:start 0 :length 1 :period 2 :params {:init :a}}
                 {:start 1N :length 1/2 :period 2 :params {:init :b}}
                 {:start 3/2 :length 1/2 :period 2 :params {:init :c}}]
                (cyc :a (fit :b :c)))))

  (testing "complex fit with multiple cyc"
    (is (match? [{:start 0N :length 1/3 :period 2 :params {:init :a}}
                 {:start 1/3 :length 1/3 :period 2 :params {:init :c}}
                 {:start 2/3 :length 1/3 :period 2 :params {:init :f}}
                 {:start 1N :length 1/3 :period 2 :params {:init :b}}
                 {:start 4/3 :length 1/6 :period 2 :params {:init :d}}
                 {:start 3/2 :length 1/6 :period 2 :params {:init :e}}
                 {:start 5/3 :length 1/3 :period 2 :params {:init :f}}]
                (fit (cyc :a :b) (cyc :c (fit :d :e)) :f))))

  (testing "complex cyc with nested patterns"
    (is (match? [{:start 0N :length 1/2 :period 6 :params {:init :a}}
                 {:start 1/2 :length 1/2 :period 6 :params {:init :b}}
                 {:start 1 :length 1 :period 6 :params {:init :c}}
                 {:start 2 :length 1 :period 6 :params {:init :f}}
                 {:start 3N :length 1/2 :period 6 :params {:init :a}}
                 {:start 7/2 :length 1/2 :period 6 :params {:init :b}}
                 {:start 4N :length 1/2 :period 6 :params {:init :d}}
                 {:start 9/2 :length 1/2 :period 6 :params {:init :e}}
                 {:start 5 :length 1 :period 6 :params {:init :f}}]
                (cyc (fit :a :b) (cyc :c (fit :d :e)) :f)))))

(deftest x-test
  (testing "x with single value"
    (is (match? [{:start 0N :length 1/2 :period 1 :params {:init :a}}
                 {:start 1/2 :length 1/2 :period 1 :params {:init :a}}]
                (x 2 :a))))

  (testing "x with multiple values"
    (is (match? [{:start 0N :length 1/4 :period 1 :params {:init :a}}
                 {:start 1/4 :length 1/4 :period 1 :params {:init :b}}
                 {:start 1/2 :length 1/4 :period 1 :params {:init :a}}
                 {:start 3/4 :length 1/4 :period 1 :params {:init :b}}]
                (x 2 :a :b))))

  (testing "x with vector"
    (is (match? [{:start 0N :length 1/4 :period 1 :params {:init :a}}
                 {:start 1/4 :length 1/4 :period 1 :params {:init :b}}
                 {:start 1/2 :length 1/4 :period 1 :params {:init :a}}
                 {:start 3/4 :length 1/4 :period 1 :params {:init :b}}]
                (x 2 [:a :b]))))

  (testing "x with fit"
    (is (match? [{:start 0N :length 1/4 :period 1 :params {:init :a}}
                 {:start 1/4 :length 1/4 :period 1 :params {:init :b}}
                 {:start 1/2 :length 1/4 :period 1 :params {:init :a}}
                 {:start 3/4 :length 1/4 :period 1 :params {:init :b}}]
                (x 2 (fit :a :b)))))

  (testing "x with range"
    (is (match? [{:start 0N :length 1/4 :period 1 :params {:init 0}}
                 {:start 1/4 :length 1/4 :period 1 :params {:init 1}}
                 {:start 1/2 :length 1/4 :period 1 :params {:init 0}}
                 {:start 3/4 :length 1/4 :period 1 :params {:init 1}}]
                (x 2 (range 2)))))

  (testing "x with vector pattern"
    (is (match? [{:start 0N :length 1/4 :period 1 :params {:init :a}}
                 {:start 1/4 :length 1/4 :period 1 :params {:init :a}}
                 {:start 1/2 :length 1/2 :period 1 :params {:init :b}}]
                (x [2 1] [:a :b]))))

  (testing "x with cyc pattern"
    (is (match? [{:start 0N :length 1/4 :period 2 :params {:init :a}}
                 {:start 1/4 :length 1/4 :period 2 :params {:init :b}}
                 {:start 1/2 :length 1/4 :period 2 :params {:init :a}}
                 {:start 3/4 :length 1/4 :period 2 :params {:init :b}}
                 {:start 1N :length 1/2 :period 2 :params {:init :a}}
                 {:start 3/2 :length 1/2 :period 2 :params {:init :b}}]
                (x (cyc 2 1) [:a :b])))))

(deftest el-test
  (testing "el basic - elongates second element"
    (is (match? [{:start 0N :length 1/3 :period 1 :params {:init :a}}
                 {:start 1/3 :length 2/3 :period 1 :params {:init :b}}]
                (fit :a (el 2 :b)))))

  (testing "el with multiple values"
    (is (match? [{:start 0N :length 1/3 :period 1 :params {:init :a}}
                 {:start 1/3 :length 1/3 :period 1 :params {:init :b}}
                 {:start 2/3 :length 1/3 :period 1 :params {:init :b}}]
                (fit :a (el 2 :b :b)))))

  (testing "el with fit"
    (is (match? [{:start 0N :length 1/3 :period 1 :params {:init :a}}
                 {:start 1/3 :length 1/3 :period 1 :params {:init :b}}
                 {:start 2/3 :length 1/3 :period 1 :params {:init :b}}]
                (fit :a (el 2 (fit :b :b))))))

  (testing "el with vector"
    (is (match? [{:start 0N :length 1/3 :period 1 :params {:init :a}}
                 {:start 1/3 :length 1/3 :period 1 :params {:init :b}}
                 {:start 2/3 :length 1/3 :period 1 :params {:init :b}}]
                (fit :a (el 2 [:b :b])))))

  (testing "el in first position"
    (is (match? [{:start 0N :length 2/3 :period 1 :params {:init :a}}
                 {:start 2/3 :length 1/3 :period 1 :params {:init :b}}]
                (fit (el 2 :a) :b))))

  (testing "el on both elements"
    (is (match? [{:start 0N :length 1/2 :period 1 :params {:init :a}}
                 {:start 1/2 :length 1/2 :period 1 :params {:init :b}}]
                (fit (el 2 :a) (el 2 :b)))))

  (testing "el nested in fit"
    (is (match? [{:start 0N :length 1/2 :period 1 :params {:init :a}}
                 {:start 1/2 :length 1/6 :period 1 :params {:init :b}}
                 {:start 2/3 :length 1/3 :period 1 :params {:init :c}}]
                (fit :a (fit :b (el 2 :c))))))

  (testing "el with cyc"
    (is (match? [{:start 0 :length 1 :period 3 :params {:init :a}}
                 {:start 1 :length 2 :period 3 :params {:init :b}}]
                (cyc :a (el 2 :b)))))

  (testing "el with cyc multiple values"
    (is (match? [{:start 0 :length 1 :period 3 :params {:init :a}}
                 {:start 1N :length 1N :period 3 :params {:init :b}}
                 {:start 2N :length 1N :period 3 :params {:init :b}}]
                (cyc :a (el 2 :b :b)))))

  (testing "el with cyc and vector"
    (is (match? [{:start 0 :length 1 :period 3 :params {:init :a}}
                 {:start 1N :length 1N :period 3 :params {:init :b}}
                 {:start 2N :length 1N :period 3 :params {:init :b}}]
                (cyc :a (el 2 [:b :b])))))

  (testing "el with cyc and fit"
    (is (match? [{:start 0 :length 1 :period 3 :params {:init :a}}
                 {:start 1N :length 1N :period 3 :params {:init :b}}
                 {:start 2N :length 1N :period 3 :params {:init :b}}]
                (cyc :a (el 2 (fit :b :b))))))

  (testing "el nested in cyc with cyc"
    (is (match? [{:start 0 :length 1 :period 6 :params {:init :a}}
                 {:start 1 :length 1 :period 6 :params {:init :b}}
                 {:start 2 :length 1 :period 6 :params {:init :a}}
                 {:start 3 :length 2 :period 6 :params {:init :c}}
                 {:start 4 :length 1 :period 6 :params {:init :a}}]
                (cyc :a (cyc :b (el 2 :c))))))

  (testing "el with vector pattern"
    (is (match? [{:start 0N :length 2/3 :period 1 :params {:init :a}}
                 {:start 2/3 :length 1/3 :period 1 :params {:init :b}}]
                (el [2 1] :a :b))))

  (testing "el with cyc pattern"
    (is (match? [{:start 0N :length 2/3 :period 2 :params {:init :a}}
                 {:start 2/3 :length 1/3 :period 2 :params {:init :b}}
                 {:start 1N :length 1/2 :period 2 :params {:init :a}}
                 {:start 3/2 :length 1/2 :period 2 :params {:init :b}}]
                (el (cyc [2 1] 1) :a :b)))))

(deftest rep-test
  (testing "rep with fit"
    (is (match? [{:start 0N :length 1/3 :period 1 :params {:init :a}}
                 {:start 1/3 :length 1/3 :period 1 :params {:init :b}}
                 {:start 2/3 :length 1/3 :period 1 :params {:init :b}}]
                (fit :a (rep 2 :b)))))

  (testing "rep with cyc"
    (is (match? [{:start 0 :length 1 :period 3 :params {:init :a}}
                 {:start 1N :length 1N :period 3 :params {:init :b}}
                 {:start 2N :length 1N :period 3 :params {:init :b}}]
                (cyc :a (rep 2 :b))))))

(deftest spl-test
  (testing "spl with fit - splits evenly"
    (is (match? [{:start 0N :length 1/3 :period 1 :params {:init :a}}
                 {:start 1/3 :length 1/3 :period 1 :params {:init :b}}
                 {:start 2/3 :length 1/3 :period 1 :params {:init :c}}]
                (fit :a (spl :b :c)))))

  (testing "spl with cyc"
    (is (match? [{:start 0 :length 1 :period 3 :params {:init :a}}
                 {:start 1N :length 1N :period 3 :params {:init :b}}
                 {:start 2N :length 1N :period 3 :params {:init :c}}]
                (cyc :a (spl :b :c)))))

  (testing "spl with el - respects weight"
    (is (match? [{:start 0N :length 1/4 :period 1 :params {:init :a}}
                 {:start 1/4 :length 1/4 :period 1 :params {:init :b}}
                 {:start 1/2 :length 1/2 :period 1 :params {:init :c}}]
                (fit :a (spl :b (el 2 :c))))))

  (testing "spl with el and cyc"
    (is (match? [{:start 0 :length 1 :period 4 :params {:init :a}}
                 {:start 1N :length 1N :period 4 :params {:init :b}}
                 {:start 2N :length 2N :period 4 :params {:init :c}}]
                (cyc :a (spl :b (el 2 :c)))))))

(deftest slow-test
  (testing "slow 2 - doubles period"
    (is (match? [{:start 0N :length 1N :period 2 :params {:init :a}}
                 {:start 1N :length 1N :period 2 :params {:init :b}}]
                (slow 2 (fit :a :b)))))

  (testing "slow 1/2 - halves period"
    (is (match? [{:start 0N :length 1/4 :period 1/2 :params {:init :a}}
                 {:start 1/4 :length 1/4 :period 1/2 :params {:init :b}}]
                (slow 1/2 (fit :a :b)))))

  (testing "slow 3/2"
    (is (match? [{:start 0N :length 3/4 :period 3/2 :params {:init :a}}
                 {:start 3/4 :length 3/4 :period 3/2 :params {:init :b}}]
                (slow 3/2 (fit :a :b)))))

  (testing "slow with cyc"
    (is (match? [{:start 0N :length 1/4 :period 1N :params {:init :a}}
                 {:start 1/4 :length 1/4 :period 1N :params {:init :b}}
                 {:start 1/2 :length 1/4 :period 1N :params {:init :a}}
                 {:start 3/4 :length 1/4 :period 1N :params {:init :c}}]
                (slow 1/4 (cyc :a (cyc :b :c)))))))

(deftest speed-test
  (testing "speed 2 with multiple args"
    (is (match? [{:start 0N :length 1/4 :period 1/2 :params {:init :a}}
                 {:start 1/4 :length 1/4 :period 1/2 :params {:init :b}}]
                (speed 2 :a :b))))

  (testing "speed 2 - halves period"
    (is (match? [{:start 0N :length 1/4 :period 1/2 :params {:init :a}}
                 {:start 1/4 :length 1/4 :period 1/2 :params {:init :b}}]
                (speed 2 (fit :a :b)))))

  (testing "speed 1/2 - doubles period"
    (is (match? [{:start 0N :length 1N :period 2N :params {:init :a}}
                 {:start 1N :length 1N :period 2N :params {:init :b}}]
                (speed 1/2 (fit :a :b)))))

  (testing "speed 3/2"
    (is (match? [{:start 0N :length 1/3 :period 2/3 :params {:init :a}}
                 {:start 1/3 :length 1/3 :period 2/3 :params {:init :b}}]
                (speed 3/2 (fit :a :b)))))

  (testing "speed with cyc"
    (is (match? [{:start 0N :length 4N :period 16N :params {:init :a}}
                 {:start 4N :length 4N :period 16N :params {:init :b}}
                 {:start 8N :length 4N :period 16N :params {:init :a}}
                 {:start 12N :length 4N :period 16N :params {:init :c}}]
                (speed 1/4 (cyc :a (cyc :b :c)))))))

(deftest degrade-test
  (testing "degrade returns functions in params"
    (let [result (degrade 1/2 (fit :a :b))]
      (is (= 2 (count result)))
      (is (every? #(fn? (get-in % [:params :init])) result)))))

(deftest euc-test
  (testing "euclidean 3 hits in 8 steps"
    (is (match? [{:start 0N :length 1/8 :period 1 :params {:init :a}}
                 {:start 3/8 :length 1/8 :period 1 :params {:init :a}}
                 {:start 3/4 :length 1/8 :period 1 :params {:init :a}}]
                (euc [3 8] :a))))

  (testing "euclidean with rotation"
    (is (match? [{:start 1/8 :length 1/8 :period 1 :params {:init :a}}
                 {:start 1/2 :length 1/8 :period 1 :params {:init :a}}
                 {:start 3/4 :length 1/8 :period 1 :params {:init :a}}]
                (euc [3 8 2] :a))))

  (testing "euclidean 5 hits in 8 steps with pattern"
    (is (match? [{:start 0N :length 1/16 :period 1 :params {:init :a}}
                 {:start 1/16 :length 1/16 :period 1 :params {:init :b}}
                 {:start 1/4 :length 1/16 :period 1 :params {:init :a}}
                 {:start 5/16 :length 1/16 :period 1 :params {:init :b}}
                 {:start 3/8 :length 1/16 :period 1 :params {:init :a}}
                 {:start 7/16 :length 1/16 :period 1 :params {:init :b}}
                 {:start 5/8 :length 1/16 :period 1 :params {:init :a}}
                 {:start 11/16 :length 1/16 :period 1 :params {:init :b}}
                 {:start 3/4 :length 1/16 :period 1 :params {:init :a}}
                 {:start 13/16 :length 1/16 :period 1 :params {:init :b}}]
                (euc [5 8] :a :b))))

  (testing "euclidean with fit"
    (is (match? [{:start 0N :length 1/16 :period 1 :params {:init :a}}
                 {:start 1/16 :length 1/16 :period 1 :params {:init :b}}
                 {:start 3/8 :length 1/16 :period 1 :params {:init :a}}
                 {:start 7/16 :length 1/16 :period 1 :params {:init :b}}
                 {:start 3/4 :length 1/16 :period 1 :params {:init :a}}
                 {:start 13/16 :length 1/16 :period 1 :params {:init :b}}]
                (euc [3 8] (fit :a :b))))))
