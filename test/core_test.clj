(ns test.core-test
  (:require [shhh.core :refer :all]
            [expectations.clojure.test :refer [defexpect expect]]))


(defexpect fast-op
  (expect [{:s :bd :whole 0 :order 1} {:s :bd :whole 1/3 :order 1} {:s :bd :whole 2/3 :order 1}]
          (assign-times [:fast {:s :bd} {:s :bd} {:s :bd}])))

(defexpect slow-op
  (expect [{:s :bd :whole 0 :order 3} {:s :bd :whole 1 :order 3} {:s :bd :whole 2 :order 3}]
          (assign-times [:slow {:s :bd} {:s :bd} {:s :bd}])))


(defexpect fast-nested-fast
  (expect [{:s :bd :whole 0 :order 1}
           [{:s :sd :whole 1/3 :order 1} {:s :sd :whole 1/2 :order 1}]
           {:s :bd :whole 2/3 :order 1}]
          (assign-times [:fast {:s :bd} [:fast {:s :sd} {:s :sd}] {:s :bd}])))

(defexpect fast-nested-slow
  (expect [{:s :bd :whole 0 :order 1}
           [{:s :sd :whole 1/3 :order 2} {:s :sd :whole 4/3 :order 2}]
           {:s :bd :whole 2/3 :order 1}]
          (assign-times [:fast {:s :bd} [:slow {:s :sd} {:s :sd}] {:s :bd}])))

(defexpect slow-nested-slow
  (expect [{:s :bd :whole 0 :order 3}
           [{:s :sd :whole 1 :order 6} {:s :sd :whole 4 :order 6}]
           {:s :bd :whole 2 :order 3}]
          (assign-times [:slow {:s :bd} [:slow {:s :sd} {:s :sd}] {:s :bd}])))

(defexpect slow-nested-fast
  (expect [[{:s :bd :whole 0 :order 3} {:s :bd :whole 1/2 :order 3}]
           {:s :sd :whole 1 :order 3}
           {:s :bd :whole 2 :order 3}]
          (assign-times [:slow [:fast {:s :bd} {:s :bd}] {:s :sd} {:s :bd}])))

(defexpect fast-complex-nested
  (expect [[{:s :sd :whole 0 :order 1} {:s :sd :whole 1/9 :order 1} {:s :sd :whole 2/9 :order 1}]
           [{:s :bd :whole 1/3 :order 3} {:s :bd :whole 4/3 :order 3} {:s :bd :whole 7/3 :order 3}]
           {:s :cr :whole 2/3 :order 1}]
          (assign-times [:fast [:fast {:s :sd} {:s :sd} {:s :sd}] [:slow {:s :bd} {:s :bd} {:s :bd}] {:s :cr}])))

(defexpect slow-nested-fast-slow
  (expect [{:s :bd :whole 0 :order 2}
           [{:s :sd :whole 1 :order 2}
            [{:s :hh :whole 3/2 :order 4} {:s :cr :whole 7/2 :order 4}]]]
          (assign-times [:slow {:s :bd} [:fast {:s :sd} [:slow {:s :hh} {:s :cr}]]])))

(defexpect fast-nested-slow-fast
  (expect [{:s :cc :whole 0 :order 1}
           [{:s :bd :whole 1/2 :order 2}
            [{:s :cr :whole 3/2 :order 2} {:s :hh :whole 7/4 :order 2}]]]
          (assign-times [:fast {:s :cc} [:slow {:s :bd} [:fast {:s :cr} {:s :hh}]]])))

(defexpect fast-complex-multi-nested
  (expect [[{:s :sd :whole 0 :order 1} [{:s :sd :whole 1/6 :order 2} {:s :sd :whole 7/6 :order 2}]]
           [[{:s :bd :whole 1/3 :order 2} [{:s :oh :whole 1/2 :order 4} {:s :ch :whole 5/2 :order 4}]] {:s :bd :whole 4/3 :order 2}]
           {:s :cr :whole 2/3 :order 1}]
          (assign-times [:fast
                         [:fast {:s :sd} [:slow {:s :sd} {:s :sd}]]
                         [:slow [:fast {:s :bd} [:slow {:s :oh} {:s :ch}]] {:s :bd}]
                         {:s :cr}])))
