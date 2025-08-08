(ns test.core-test
  (:require [shhh.core :refer :all]
            [shhh.pattern :refer [rep times cycle]]
            [expectations.clojure.test :refer [defexpect expect]]))


(defexpect test-s->cycles
  (reset! cps 1)
  (expect (s->cycles 0) 0)
  (expect (s->cycles 1) 1)
  (expect (s->cycles 1.5) 1.5)

  (reset! cps 2)
  (expect (s->cycles 0) 0)
  (expect (s->cycles 1) 2)
  (expect (s->cycles 3/2) 3)

  (reset! cps 1/2)
  (expect (s->cycles 0) 0)
  (expect (s->cycles 1) 1/2)
  (expect (s->cycles 3/2) 3/4))


(defexpect test-s->pos
  (reset! cps 1)
  (expect (s->pos 0 1) 0)
  (expect (s->pos 1 1) 0)
  (expect (s->pos 3/2 1) 1/2)

  (reset! cps 1)
  (expect (s->pos 1 2) 1)
  (expect (s->pos 3/2 2) 3/2)
  (expect (s->pos 5/2 2) 1/2)

  (reset! cps 2)
  (expect (s->pos 0 1) 0)
  (expect (s->pos 1/4 1) 1/2)
  (expect (s->pos 1 1) 0)
  (expect (s->pos 3/2 1) 0)

  (reset! cps 2)
  (expect (s->pos 3/4 2) 3/2)
  (expect (s->pos 1/2 2) 1)
  (expect (s->pos 1 2) 0)
  (expect (s->pos 5/4 2) 1/2)

  (reset! cps 1/2)
  (expect (s->pos 5/4 2) 5/8)
  (expect (s->pos 2 2) 1)
  (expect (s->pos 3 2) 3/2)
  (expect (s->pos 4 2) 0)
  (expect (s->pos 5 2) 1/2))
