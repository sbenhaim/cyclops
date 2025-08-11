(ns pattern-test
  (:require
   [cyclops.pattern :refer :all]
   [expectations.clojure.test :refer [defexpect expect in]]))

;; Test helper functions
(defn event-at [events pos]
  (first (filter #(= (:position %) pos) events)))


(defn pp [pat]
  (->> pat
      process-pattern
      (map #(select-keys % [:start :length :period :value]))))

;; Fit operation tests
(defexpect fit-basic
  (expect [{:start 0 :length 1/3 :period 1   :value 0}
           {:start 1/3 :length 1/3 :period 1 :value 1}
           {:start 2/3 :length 1/3 :period 1 :value 2}]
          (pp (range 3))))

(defexpect fit-nested
  (expect [{:start 0 :length 1/4 :period 1  :value :a}
           {:start 1/4 :length 1/4 :period 1 :value :b}
           {:start 1/2 :length 1/2 :period 1 :value :c}]
          (pp [[:a :b] :c])))


;; Times operation tests
(defexpect times-basic
  (expect [{:start 0 :length 1/6 :period 1   :value :a}
           {:start 1/6 :length 1/6 :period 1 :value :b}
           {:start 2/6 :length 1/6 :period 1 :value :a}
           {:start 3/6 :length 1/6 :period 1 :value :b}
           {:start 4/6 :length 1/6 :period 1 :value :a}
           {:start 5/6 :length 1/6 :period 1 :value :b}]
          (pp (times 3 :a :b))))

(defexpect times-nested
  (expect [{:start 0 :length 1/4 :period 1   :value :a}
           {:start 1/4 :length 1/4 :period 1 :value :b}
           {:start 2/4 :length 1/4 :period 1 :value :a}
           {:start 3/4 :length 1/4 :period 1 :value :b}]
          (pp (times 2 [:a :b]))))

;; Cycle operation tests
(defexpect cyc-basic
  (expect [{:start 0 :length 1 :period 3 :value :a}
           {:start 1 :length 1 :period 3 :value :b}
           {:start 2 :length 1 :period 3 :value :c}]
          (pp (cyc :a :b :c))))


(defexpect cyc-nested
  (expect [{:start 0 :length 1/2 :period 2 :value :a}
           {:start 1/2 :length 1/2 :period 2 :value :b}
           {:start 1 :length 1 :period 2 :value :c}]
          (pp (cyc [:a :b] :c)))
  (expect [{:start 0 :length 1/2 :period 2 :value :a}
           {:start 1 :length 1/2 :period 2 :value :b}
           {:start 1/2 :length 1/2 :period 1 :value :c}]
          (pp [(cyc :a :b) :c])))

;; Slow operation tests
(defexpect slow-basic
  (expect [{:start 0 :length 2/3 :period 2 :value :a}
           {:start 2/3 :length 2/3 :period 2 :value :b}
           {:start 4/3 :length 2/3 :period 2 :value :c}]
          (pp (slow 2 :a :b :c)))
  (expect [{:start 0 :length 1 :period 2 :value :a}
           {:start 1 :length 1 :period 2 :value :b}]
          (pp (slow 2 [:a :b]))))

(defexpect slow-nested
  (expect [{:start 0 :length 1 :period 2 :value :a}
           {:start 1 :length 1 :period 2 :value :b}]
          (pp (slow 2 [:a :b]))))

;; Splice operation tests
(defexpect splice-basic
  (expect [{:start 0 :length 1/3 :period 1 :value :a}
           {:start 1/3 :length 1/3 :period 1 :value :b}
           {:start 2/3 :length 1/3 :period 1 :value :c}]
          (pp (spl :a :b :c))))

(defexpect splice-nested
  (expect [{:start 0 :length 1/4 :period 1 :value :a}
           {:start 1/4 :length 1/4 :period 1 :value :b}
           {:start 2/4 :length 1/4 :period 1 :value :c}
           {:start 3/4 :length 1/4 :period 1 :value :d}]
          (pp [(spl :a :b) (spl :c :d)]))
  (expect [{:start 0 :length 1 :period 4 :value :a}
           {:start 1 :length 1 :period 4 :value :b}
           {:start 2 :length 1 :period 4 :value :c}
           {:start 3 :length 1 :period 4 :value :d}]
          (pp (cyc (spl :a :b) (spl :c :d)))))

;; Rep operation tests
(defexpect rep-basic
  (expect [{:start 0 :length 1/6 :period 1 :value :a}
           {:start 1/6 :length 1/6 :period 1 :value :b}
           {:start 2/6 :length 1/6 :period 1 :value :a}
           {:start 3/6 :length 1/6 :period 1 :value :b}
           {:start 4/6 :length 1/6 :period 1 :value :a}
           {:start 5/6 :length 1/6 :period 1 :value :b}]
          (pp (rep 3 :a :b))))

(defexpect rep-nested
  (expect [{:start 0 :length 1/5 :period 1 :value :a}
           {:start 1/5 :length 1/5 :period 1 :value :b}
           {:start 2/5 :length 1/5 :period 1 :value :a}
           {:start 3/5 :length 1/5 :period 1 :value :b}
           {:start 4/5 :length 1/5 :period 1 :value :c}]
          (pp [(rep 2 :a :b) :c])))

;; Elongate operation tests
(defexpect elongate-basic
  (expect [{:start 0 :length 2/3 :period 1 :value :a}
           {:start 2/3 :length 1/3 :period 1 :value :b}]
          (pp [(elongate 2 :a) :b]))

  (expect [{:start 0 :length 1/2 :period 1 :value :a}
           {:start 1/2 :length 1/2 :period 1 :value :b}]
          (pp (elongate 2 [:a :b])))

  (expect [{:start 0 :length 1/3 :period 1 :value :a}
           {:start 1/3 :length 1/3 :period 1 :value :b}
           {:start 2/3 :length 1/3 :period 1 :value :c}]
          (pp (elongate 2 :a :b :c))))

(defexpect elongate-nested
  (expect [{:start 0 :length 2 :period 5 :value :a}
           {:start 2 :length 2 :period 5 :value :b}
           {:start 4 :length 1 :period 5 :value :c}]
          (pp (cyc (elongate 2 :a :b) :c)))
  (expect [{:start 0 :length 1/7 :period 1 :value :a}
           {:start 1/7 :length 2/7 :period 1 :value :b}
           {:start 3/7 :length 2/7 :period 1 :value :c}
           {:start 5/7 :length 2/7 :period 1 :value :d}]
          (pp [:a (elongate 2 :b :c :d)])))
