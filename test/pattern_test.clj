(ns pattern-test
  (:require
   [shhh.pattern :refer :all]
   [expectations.clojure.test :refer [defexpect expect in]]))

;; Test helper functions
(defn event-at [events pos]
  (first (filter #(= (:position %) pos) events)))

;; Basic event conversion tests
(defexpect event-conversion
  (expect {:s "a"} (->event :a))
  (expect {:s "hello"} (->event "hello"))
  (expect {:n 1.0} (->event 1))
  (expect {:fn +} (->event +)))

;; Fit operation tests
(defexpect fit-basic
  (expect [{:position 0 :length 1/3 :period 1 :s "a"}
           {:position 1/3 :length 1/3 :period 1 :s "b"}
           {:position 2/3 :length 1/3 :period 1 :s "c"}]
          (process-pattern (fit :a :b :c))))

(defexpect fit-nested
  (expect [[{:position 0 :length 1/4 :period 1 :s "a"}
            {:position 1/4 :length 1/4 :period 1 :s "b"}]
           {:position 1/2 :length 1/2 :period 1 :s "c"}]
          (process-pattern (fit (fit :a :b) :c))))


;; Times operation tests
(defexpect times-basic
  (expect [{:position 0 :length 1/6 :period 1 :s "a"}
           {:position 1/6 :length 1/6 :period 1 :s "b"}
           {:position 2/6 :length 1/6 :period 1 :s "a"}
           {:position 3/6 :length 1/6 :period 1 :s "b"}
           {:position 4/6 :length 1/6 :period 1 :s "a"}
           {:position 5/6 :length 1/6 :period 1 :s "b"}]
          (process-pattern (times 3 :a :b))))

(defexpect times-nested
  (expect [[{:position 0 :length 1/4 :period 1 :s "a"}
            {:position 1/4 :length 1/4 :period 1 :s "b"}]
           [{:position 2/4 :length 1/4 :period 1 :s "a"}
            {:position 3/4 :length 1/4 :period 1 :s "b"}]]
          (process-pattern (times 2 (fit :a :b)))))

;; Cycle operation tests
(defexpect cycle-basic
  (expect [{:position 0 :length 1 :period 3 :s "a"}
           {:position 1 :length 1 :period 3 :s "b"}
           {:position 2 :length 1 :period 3 :s "c"}]
          (process-pattern (cycle :a :b :c))))

(defexpect cycle-nested
  (expect [[{:position 0 :length 1/2 :period 2 :s "a"}
            {:position 1/2 :length 1/2 :period 2 :s "b"}]
           {:position 1 :length 1 :period 2 :s "c"}]
          (process-pattern (cycle (fit :a :b) :c)))
  (expect [[{:position 0 :length 1/2 :period 2 :s "a"}
            {:position 1 :length 1/2 :period 2 :s "b"}]
           {:position 1/2 :length 1/2 :period 1 :s "c"}]
          (process-pattern (fit (cycle :a :b) :c))))

;; Slow operation tests
(defexpect slow-basic
  (expect [{:position 0 :length 2/3 :period 2 :s "a"}
           {:position 2/3 :length 2/3 :period 2 :s "b"}
           {:position 4/3 :length 2/3 :period 2 :s "c"}]
          (process-pattern (slow 2 :a :b :c)))
  (expect [[{:position 0 :length 1 :period 2 :s "a"}
            {:position 1 :length 1 :period 2 :s "b"}]]
          (process-pattern (slow 2 (fit :a :b)))))

(defexpect slow-nested
  (expect [[{:position 0 :length 1 :period 2 :s "a"}
            {:position 1 :length 1 :period 2 :s "b"}]]
          (process-pattern (slow 2 (fit :a :b)))))

;; Splice operation tests
(defexpect splice-basic
  (expect [{:position 0 :length 1/3 :period 1 :s "a"}
           {:position 1/3 :length 1/3 :period 1 :s "b"}
           {:position 2/3 :length 1/3 :period 1 :s "c"}]
          (process-pattern (splice :a :b :c))))

(defexpect splice-nested
  (expect [[{:position 0 :length 1/4 :period 1 :s "a"}
            {:position 1/4 :length 1/4 :period 1 :s "b"}]
           [{:position 2/4 :length 1/4 :period 1 :s "c"}
            {:position 3/4 :length 1/4 :period 1 :s "d"}]]
          (process-pattern (fit (splice :a :b) (splice :c :d))))
  (expect [[{:position 0 :length 1 :period 4 :s "a"}
            {:position 1 :length 1 :period 4 :s "b"}]
           [{:position 2 :length 1 :period 4 :s "c"}
            {:position 3 :length 1 :period 4 :s "d"}]]
          (process-pattern (cycle (splice :a :b) (splice :c :d)))))

;; Rep operation tests
(defexpect rep-basic
  (expect [{:position 0 :length 1/6 :period 1 :s "a"}
           {:position 1/6 :length 1/6 :period 1 :s "b"}
           {:position 2/6 :length 1/6 :period 1 :s "a"}
           {:position 3/6 :length 1/6 :period 1 :s "b"}
           {:position 4/6 :length 1/6 :period 1 :s "a"}
           {:position 5/6 :length 1/6 :period 1 :s "b"}]
          (process-pattern (rep 3 :a :b))))

(defexpect rep-nested
  (expect [[{:position 0 :length 1/5 :period 1 :s "a"}
            {:position 1/5 :length 1/5 :period 1 :s "b"}
            {:position 2/5 :length 1/5 :period 1 :s "a"}
            {:position 3/5 :length 1/5 :period 1 :s "b"}]
           {:position 4/5 :length 1/5 :period 1 :s "c"}]
          (process-pattern (fit (rep 2 :a :b) :c))))

;; Elongate operation tests
(defexpect elongate-basic
  (expect [[{:position 0 :length 2/3 :period 1 :s "a"}]
           {:position 2/3 :length 1/3 :period 1 :s "b"}]
          (process-pattern (fit (elongate 2 :a) :b)))

  (expect [[{:position 0 :length 1/2 :period 1 :s "a"}
            {:position 1/2 :length 1/2 :period 1 :s "b"}]]
          (process-pattern (elongate 2 [:a :b])))

  (expect [{:position 0 :length 1/3 :period 1 :s "a"}
           {:position 1/3 :length 1/3 :period 1 :s "b"}
           {:position 2/3 :length 1/3 :period 1 :s "c"}]
          (process-pattern (elongate 2 :a :b :c))))

(defexpect elongate-nested
  (expect [[{:position 0 :length 2 :period 5 :s "a"}
            {:position 2 :length 2 :period 5 :s "b"}]
           {:position 4 :length 1 :period 5 :s "c"}]
          (process-pattern (cycle (elongate 2 :a :b) :c)))
  (expect [{:position 0 :length 1/7 :period 1 :s "a"}
           [{:position 1/7 :length 2/7 :period 1 :s "b"}
            {:position 3/7 :length 2/7 :period 1 :s "c"}
            {:position 5/7 :length 2/7 :period 1 :s "d"}]]
          (process-pattern (fit :a (elongate 2 :b :c :d)))))

