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

(defexpect test-slice
  (expect (slice [1 [{:position 0 :s "a"} {:position 1/2 :s "b"}]] 0 1/2)
          [{:position 0 :s "a"}])
  (expect (slice [1 [{:position 0 :s "a"} {:position 1/2 :s "b"}]] 1/2 1)
          [{:position 0 :s "b"}])
  (expect (slice [1 [{:position 0 :s "a"} {:position 1/2 :s "b"}]] 1/2 3/2)
          [{:position 0 :s "b"} {:position 1/2 :s "a"}])
  (expect (slice [1 [{:position 0 :s "a"} {:position 1/2 :s "b"}]] 1/2 1/2)
          [{:position 0 :s "b"} {:position 1/2 :s "a"}])

  (expect (slice [2 [{:position 0 :s "a"} {:position 1/2 :s "b"}
                     {:position 1 :s "c"} {:position 3/2 :s "d"}]]
                 0 1)
          [{:position 0 :s "a"} {:position 1/2 :s "b"}])
  (expect (slice [2 [{:position 0 :s "a"} {:position 1/2 :s "b"}
                     {:position 1 :s "c"} {:position 3/2 :s "d"}]]
                 1/2 3/2)
          [{:position 0 :s "b"} {:position 1/2 :s "c"}])
  (expect (slice [2 [{:position 0 :s "a"} {:position 1/2 :s "b"}
                     {:position 1 :s "c"} {:position 3/2 :s "d"}]]
                 1 5/2)
          [{:position 0 :s "c"} {:position 1/2 :s "d"} {:position 1 :s "a"}])
  (expect (slice [2 [{:position 0 :s "a"} {:position 1/2 :s "b"}
                     {:position 1 :s "c"} {:position 3/2 :s "d"}]]
                 3/4 1/2)
          [{:position 1/4 :s "c"} {:position 3/4 :s "d"} {:position 5/4 :s "a"}])
  (expect (slice [1 [{:position 0 :s "a"} {:position 1/2 :s "b"}]]
                 1/2 3)
          [{:position 0 :s "b"} {:position 1/2 :s "a"} {:position 1 :s "b"} {:position 3/2 :s "a"} {:position 2 :s "b"}]))
