(ns test.shhh
  (:require [shhh.shhh :refer all]))


(comment

  (assert (= (process-pattern [:fast {:s :bd} {:s :bd} {:s :bd}])
             {1 [{:s :bd :whole 0 :order 1} {:s :bd :whole 1/3 :order 1} {:s :bd :whole 2/3 :order 1}]}))

  ;; TODO: Fix add order
  (assert (= (time-events [:slow {:s :bd} {:s :bd} {:s :bd}] 1 1 0)
             '({:s :bd :whole 0} {:s :bd :whole 1} {:s :bd :whole 2})))

  (assert (= (process-pattern [:fast {:s :bd} [:fast {:s :sd} {:s :sd}] {:s :bd}])
             '({:s :bd :whole 0} ({:s :sd :whole 1/3} {:s :sd :whole 1/2}) {:s :bd :whole 2/3})))

  (assert (= (process-pattern [:fast {:s :bd} [:slow {:s :sd} {:s :sd}] {:s :bd}])
             '({:s :bd :whole 0} ({:s :sd :whole 1/3} {:s :sd :whole 4/3}) {:s :bd :whole 2/3})))

  (assert (= (time-events [:slow {:s :bd} [:slow {:s :sd} {:s :sd}] {:s :bd}] 1 1 0)
             '({:s :bd :whole 0} ({:s :sd :whole 1} {:s :sd :whole 4}) {:s :bd :whole 2})))

  (assert (= (time-events [:slow [:fast {:s :bd} {:s :bd}] {:s :sd} {:s :bd}] 1 1 0)
             '(({:s :bd :whole 0} {:s :bd :whole 1/2}) {:s :sd :whole 1} {:s :bd :whole 2})))

  (assert (= (time-events [:fast [:fast {:s :sd} {:s :sd} {:s :sd}] [:slow {:s :bd} {:s :bd} {:s :bd}] {:s :cr}] 1 1 0)

             '(({:s :sd :whole 0} {:s :sd :whole 1/9} {:s :sd :whole 2/9})
               ({:s :bd :whole 1/3} {:s :bd :whole 4/3} {:s :bd :whole 7/3})
               {:s :cr :whole 2/3})))


  (assert (= (time-events [:slow {:s :bd} [:fast {:s :sd} [:slow {:s :hh} {:s :cr}]]] 1 1 0)
             ;; < sd [ sd < sd sd > ] >
             '({:s :bd :whole 0}
               ({:s :sd :whole 1}
                ({:s :hh :whole 3/2} {:s :cr :whole 7/2})))))


  (assert (= (time-events [:fast
                           {:s :cc}
                           [:slow {:s :bd} [:fast {:s :cr} {:s :hh}]]] 1 1 0)
             ;; cc <bd [cr hh]> cc:0 bd:1/2 cc:3/2 hh:7/4
             '({:s :cc :whole 0}
               ({:s :bd :whole 1/2} ({:s :cr :whole 3/2} {:s :hh :whole 7/4})))))


  (assert (= (time-events [:fast
                           [:fast {:s :sd} [:slow {:s :sd} {:s :sd}]]
                           [:slow [:fast {:s :bd} [:slow {:s :oh} {:s :ch}]] {:s :bd}]
                           {:s :cr}] 1 1 0)
             `(({:s :sd :whole 0} ({:s :sd :whole 1/6} {:s :sd :whole 7/6}))
               (({:s :bd :whole 1/3} ({:s :oh :whole 1/2} {:s :ch :whole 5/2})) {:s :bd :whole 4/3})
               {:s :cr :whole 2/3})))
  ,)
