(ns ops-test
  (:require
   [cyclops.ops :refer :all]
   [expectations.clojure.test :refer [defexpect expect in]]))


;; Fit operation tests
(defexpect test-fit
  (expect [{:start 0 :length 1/3 :init 0}
           {:start 1/3 :length 1/3 :init 1}
           {:start 2/3 :length 1/3 :init 2}]
          (-> (range 3) fit evts))
  (expect [{:start 0 :length 1/4  :init :a}
           {:start 1/4 :length 1/4 :init :b}
           {:start 1/2 :length 1/2 :init :c}]
          (evts (fit [[:a :b] :c]))))


(defexpect test-x
  (expect [{:start 0 :length 1/6   :init :a}
           {:start 1/6 :length 1/6 :init :b}
           {:start 2/6 :length 1/6 :init :a}
           {:start 3/6 :length 1/6 :init :b}
           {:start 4/6 :length 1/6 :init :a}
           {:start 5/6 :length 1/6 :init :b}]
          (evts (x 3 :a :b))))

(defexpect times-nested
  (expect [{:start 0 :length 1/4   :init :a}
           {:start 1/4 :length 1/4 :init :b}
           {:start 2/4 :length 1/4 :init :a}
           {:start 3/4 :length 1/4 :init :b}]
          (pp (times 2 [:a :b]))))

;; Cycle operation tests
(defexpect cyc-basic
  (expect [{:start 0 :length 1 :init :a}
           {:start 1 :length 1 :init :b}
           {:start 2 :length 1 :init :c}]
          (pp (cyc :a :b :c))))


(defexpect cyc-nested
  (expect [{:start 0 :length 1/2 :init :a}
           {:start 1/2 :length 1/2 :init :b}
           {:start 1 :length 1 :init :c}]
          (pp (cyc [:a :b] :c)))
  (expect [{:start 0 :length 1/2 :init :a}
           {:start 1 :length 1/2 :init :b}
           {:start 1/2 :length 1/2 :init :c}]
          (pp [(cyc :a :b) :c])))

;; Slow operation tests
(defexpect slow-basic
  (expect [{:start 0 :length 2/3 :init :a}
           {:start 2/3 :length 2/3 :init :b}
           {:start 4/3 :length 2/3 :init :c}]
          (pp (slow 2 :a :b :c)))
  (expect [{:start 0 :length 1 :init :a}
           {:start 1 :length 1 :init :b}]
          (pp (slow 2 [:a :b]))))

(defexpect slow-nested
  (expect [{:start 0 :length 1 :init :a}
           {:start 1 :length 1 :init :b}]
          (pp (slow 2 [:a :b]))))

;; Splice operation tests
(defexpect splice-basic
  (expect [{:start 0 :length 1/3 :init :a}
           {:start 1/3 :length 1/3 :init :b}
           {:start 2/3 :length 1/3 :init :c}]
          (pp (spl :a :b :c))))

(defexpect splice-nested
  (expect [{:start 0 :length 1/4 :init :a}
           {:start 1/4 :length 1/4 :init :b}
           {:start 2/4 :length 1/4 :init :c}
           {:start 3/4 :length 1/4 :init :d}]
          (pp [(spl :a :b) (spl :c :d)]))
  (expect [{:start 0 :length 1 :init :a}
           {:start 1 :length 1 :init :b}
           {:start 2 :length 1 :init :c}
           {:start 3 :length 1 :init :d}]
          (pp (cyc (spl :a :b) (spl :c :d)))))

;; Rep operation tests
(defexpect rep-basic
  (expect [{:start 0 :length 1/6 :init :a}
           {:start 1/6 :length 1/6 :init :b}
           {:start 2/6 :length 1/6 :init :a}
           {:start 3/6 :length 1/6 :init :b}
           {:start 4/6 :length 1/6 :init :a}
           {:start 5/6 :length 1/6 :init :b}]
          (pp (rep 3 :a :b))))

(defexpect rep-nested
  (expect [{:start 0 :length 1/5 :init :a}
           {:start 1/5 :length 1/5 :init :b}
           {:start 2/5 :length 1/5 :init :a}
           {:start 3/5 :length 1/5 :init :b}
           {:start 4/5 :length 1/5 :init :c}]
          (pp [(rep 2 :a :b) :c])))

;; Elongate operation tests
(defexpect elongate-basic
  (expect [{:start 0 :length 2/3 :init :a}
           {:start 2/3 :length 1/3 :init :b}]
          (pp [(elongate 2 :a) :b]))

  (expect [{:start 0 :length 1/2 :init :a}
           {:start 1/2 :length 1/2 :init :b}]
          (pp (elongate 2 [:a :b])))

  (expect [{:start 0 :length 1/3 :init :a}
           {:start 1/3 :length 1/3 :init :b}
           {:start 2/3 :length 1/3 :init :c}]
          (pp (elongate 2 :a :b :c))))

(defexpect elongate-nested
  (expect [{:start 0 :length 2 :init :a}
           {:start 2 :length 2 :init :b}
           {:start 4 :length 1 :init :c}]
          (pp (cyc (elongate 2 :a :b) :c)))
  (expect [{:start 0 :length 1/7 :init :a}
           {:start 1/7 :length 2/7 :init :b}
           {:start 3/7 :length 2/7 :init :c}
           {:start 5/7 :length 2/7 :init :d}]
          (pp [:a (elongate 2 :b :c :d)])))
