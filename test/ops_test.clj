(ns ops-test
  (:require
   [cycl.ops :refer :all]
   [clojure.test :as t :refer [deftest is]]
   [matcher-combinators.test]
   [matcher-combinators.matchers :as m]))


(deftest match-bald-fit
  (let [n 5]
    (is (match? (for [i (range n)] {:start (/ i n)})
                (evts (fit (range n)))))))


(deftest match-bald-cyc
  (let [n 5]
    (is (match? (for [i (range n)] {:start i})
                (evts (cyc (range n)))))))


(deftest fit-cyc-nesting
  (is (match? (evts (fit (range 3)))
              (evts (cyc (fit (range 3)))))
      "Fit group should be treated as unit by cyc and is unchanged.")

  (is (match? (evts (cyc (range 5)))
              (evts (fit (cyc (range 5)))))
      "Cyc group should be treated as unit by fit and is unchanged.")

  (is (match? [{:start 0 :params {:init :a} :period 3}
               {:start 1/2 :params {:init :b} :period 3}
               {:start 1 :params {:init :c} :period 3}
               {:start 2 :params {:init :d} :period 3}] 
              (evts (cyc [:a :b] :c :d))))

  (is (match? [{:start 0 :params {:init :a} :period 2}
               {:start 1/3 :params {:init :b} :period 2}
               {:start 2/3 :params {:init :c} :period 2}
               {:start 1 :params {:init :a} :period 2}
               {:start 4/3 :params {:init :b} :period 2}
               {:start 5/3 :params {:init :d} :period 2}] 
              (evts (fit :a :b (cyc :c :d))))))


(deftest x1-test
  (is (match? [{:start 0} {:start 1/2}]
              (evts (x 2 :a))))
  (is (match? [{:start 0} {:start 1/4} {:start 1/2} {:start 3/4}]
              (evts (x 2 :a :b))))
  (is (match? [{:start 0 :period 2} {:start 1/2 :period 2}
               {:start 1 :period 2} {:start 3/2 :period 2}]
              (evts (x 2 (cyc :a :b))))))


(deftest x*-test
  (is (match? [{:start 0 :params {:init :a}}
               {:start 1/4 :params {:init :b}}
               {:start 1/2  :params {:init :a}}
               {:start 3/4 :params {:init :b}}]
              (evts (x 2 :a :b))))
  (is (match? [{:start 0 :params {:init :a}}
               {:start 1/4 :params {:init :a}}
               {:start 1/2  :params {:init :b}}
               {:start 3/4 :params {:init :b}}]
              (evts (x [2 2] :a :b))))
  (is (match? [{:start 0 :params {:init :a}}
               {:start 1/4 :params {:init :a}}
               {:start 1/2  :params {:init :b}}
               {:start 2/3 :params {:init :b}}
               {:start 5/6 :params {:init :b}}]
              (evts (x [2 3] :a :b))))
  (is (match? [{:start 0 :params {:init :a}}
               {:start 1/2 :params {:init :b}}
               {:start 1  :params {:init :b}}
               {:start 5/4 :params {:init :b}}
               {:start 3/2 :params {:init :a}}
               {:start 7/4 :params {:init :b}}]
              (evts (x (cyc 1 2) :a :b))))
  (is (match? [{:start 0 :period 2 :params {:init :a}}
               {:start 1/4 :period 2 :params {:init :a}}
               {:start 1/2 :period 2 :params {:init :a}}
               {:start 2/3 :period 2 :params {:init :a}}
               {:start 5/6 :period 2 :params {:init :a}}
               {:start 1 :period 2 :params   {:init :b}}
               {:start 5/4 :period 2 :params {:init :b}}
               {:start 3/2 :period 2 :params {:init :b}}
               {:start 4/3 :period 2 :params {:init :b}}
               {:start 11/6 :period 2 :params {:init :b}}]
              (evts (x [2 3] (cyc :a :b)))))
  (is (match? [{:start 0   :period 2 :params {:init :a}}
               {:start 1/2 :period 2 :params {:init :a}}
               {:start 1   :period 2 :params {:init :b}}
               {:start 4/3 :period 2 :params {:init :b}}
               {:start 5/3 :period 2 :params {:init :b}}]
              (evts (x (cyc 2 3) (cyc :a :b))))))

(deftest rep1-test
  (is (match? [{:start 0 :params {:init :a}}
               {:start 1/3 :params {:init :b}}
               {:start 2/3 :params {:init :b}}]
              (evts (fit :a (rep 2 :b)))))
  (is (match? [{:start 0 :params {:init :a}}
               {:start 1 :params {:init :b}}
               {:start 2 :params {:init :b}}]
              (evts (cyc :a (rep 2 :b)))))
  (is (match? [{:start 0 :params {:init :a}}
               {:start 1 :params {:init :b}}
               {:start 3/2 :params {:init :c}}
               {:start 2 :params {:init :b}}
               {:start 5/2 :params {:init :c}}]
              (evts (cyc :a (rep 2 :b :c)))))
  (is (match? [{:start 0 :params {:init :a}}
               {:start 1 :params {:init :b}}
               {:start 2 :params {:init :c}}
               {:start 3 :params {:init :b}}
               {:start 4 :params {:init :c}}]
              (evts (cyc :a (rep 2 (cyc :b :c))))))
  (is (match? [{:start 0   :params {:init :a}}
               {:start 1/2 :params {:init :b}}
               {:start 1   :params {:init :a}}
               {:start 3/2 :params {:init :c}}
               {:start 2   :params {:init :a}}
               {:start 5/2 :params {:init :b}}
               {:start 3   :params {:init :a}}
               {:start 7/2 :params {:init :c}}]
              (evts (fit :a (rep 2 (cyc :b :c)))))))


(deftest rep*-test
  (is (match? [{:start 0 :params {:init :a}}
               {:start 1/6 :params {:init :b}}
               {:start 2/6 :params {:init :b}}
               {:start 3/6 :params {:init :c}}
               {:start 4/6 :params {:init :c}}
               {:start 5/6 :params {:init :c}}]
              (evts (fit :a (rep [2 3] :b :c)))))
  (is (match? [{:start 0 :params {:init :a}}
               {:start 1 :params {:init :b}}
               {:start 2 :params {:init :b}}
               {:start 3 :params {:init :c}}
               {:start 4 :params {:init :c}}
               {:start 5 :params {:init :c}}]
              (evts (cyc :a (rep [2 3] :b :c)))))
  #_(is (match? [{:start 0 :params {:init :a}}
               {:start 1/2 :params {:init :b}}
               {:start 1 :params {:init :a}}
               {:start 3/2 :params {:init :c}}
               {:start 2 :params {:init :a}}
               {:start 5/2 :params {:init :c}}]
              (evts (fit :a (rep [1 2] (cyc :b :c))))))
  (is (match? [{:start 0 :params {:init :a}}
               {:start 1/2 :params {:init :b}}
               {:start 1 :params {:init :a}}
               {:start 3/2 :params {:init :c}}
               {:start 2 :params {:init :a}}
               {:start 5/2 :params {:init :c}}]
              (evts (fit :a (rep (cyc 1 2) (cyc :b :c)))))))


(deftest slow-test
  (is (match? [{:start 0 :period 2}]
              (evts (slow 2 :a))))
  (is (match? [{:start 0 :period 2 :params {:init :a}}
              {:start 1/2 :period 2 :params {:init :b}}
              {:start 1 :period 2 :params {:init :a}}]
             (evts (fit :a (slow 2 :b))))))

(evts (fit :a (slow 2 :b) (slow 3 :c)))


(slow 2 (fit :a (cyc :b :c)))

;; Questions
;; - Should ops operate on patterns or cycles
;; - or both?

(/ 1.0 1.5)

