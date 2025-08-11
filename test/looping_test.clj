(ns looping-test
  (:require
   [cyclops.looping :refer :all]
   [cyclops.pattern :refer [process-pattern] :as p]
   [expectations.clojure.test :refer [defexpect expect]]))


(defn p->l [p]
  (-> p pattern->loops first))



(defexpect cycle-loop-test
  (expect [{:start 1/2 :end 1} {:start 3/2 :end 2} {:start 5/2 :end 3}]
          (cycle-loop 3 [1 [{:start 1/2 :end 1}]]))
  (expect [{:start 1/2 :end 3/2} {:start 3/2 :end 2} {:start 5/2 :end 7/2} {:start 7/2 :end 4}]
          (cycle-loop 2 [2 [{:start 1/2 :end 3/2} {:start 3/2 :end 2}]])))


(defexpect lcm-loops-test
  (expect [[1 [{:start 0 :length 1/2 :end 1/2 :n 2.0 :period 1} {:start 1/2 :length 1/2 :period 1 :end 1 :n 3.0}]]
           [1 [{:start 0 :end 1/3 :length 1/3 :n 4.0 :period 1}
               {:start 1/3 :length 1/3 :period 1 :end 2/3 :n 5.0}
               {:start 2/3 :length 1/3 :period 1 :end 1 :n 6.0}]]]
          (lcm-loops (p->l [2 3]) (p->l [4 5 6]))))
  (expect [[2 [{:start 1/2 :end 1} {:start 3/2 :end 2}]]
           [2 [{:start 1/2 :end 1} {:start 3/2 :end 2 }]]]
          (lcm-loops [1 [{:start 1/2 :end 1}]] [2 [{:start 1/2 :end 1} {:start 3/2 :end 2}]]))
  (expect [[6 [{:start 1/2 :end 1} {:start 3/2 :end 2} {:start 5/2 :end 3}
               {:start 7/2 :end 4} {:start 9/2 :end 5} {:start 11/2 :end 6}]]
           [6 [{:start 0 :end 1} {:start 1 :end 2} {:start 2 :end 3} {:start 3 :end 4} {:start 4 :end 5} {:start 5 :end 6}]]]
          (lcm-loops [2 [{:start 1/2 :end 1} {:start 3/2 :end 2}]]
                     [3 [{:start 0 :end 1} {:start 1 :end 2} {:start 2 :end 3}]])))


(defexpect slice-test
  (let [loop [1 [{:start 0 :end 1/4 :n 0} {:start 1/4 :end 1/2 :n 1}
                 {:start 1/2 :end 3/4 :n 2} {:start 3/4 :end 1 :n 3}]]]
    (expect [{:start 0 :n 0 :end 1/4}]
            (slice loop 0 1/4))
    (expect [{:start 0   :n 0 :end 1/4}
             {:start 1/4 :n 1 :end 1/2}
             {:start 1/2 :n 2 :end 3/4}
             {:start 3/4 :n 3 :end 1}
             {:start 1   :n 0 :end 5/4}
             {:start 5/4 :n 1 :end 3/2}
             {:start 3/2 :n 2 :end 7/4}
             {:start 7/4 :n 3 :end 2}]
            (slice loop 0 2 :offset? true :mode :begin))
    (expect [{:start 0 :n 0 :end 1/4} {:start 1/4 :n 1 :end 1/2}]
            (slice loop 0 1/2))
    (expect [{:start 0 :n 2 :end 1/4} {:start 1/4 :end 1/2 :n 3} {:start 1/2 :end 3/4 :n 0} {:start 3/4 :end 1 :n 1}]
            (slice loop 1/2 1/2 :offset? true))
    (expect [{:start 0 :end 1/4 :n 0} {:start 1/4 :end 1/2 :n 1} {:start 1/2 :end 3/4 :n 2} {:start 3/4 :end 1 :n 3}
             {:start 1 :end 5/4 :n 0} {:start 5/4 :end 3/2 :n 1} {:start 3/2 :end 7/4 :n 2} {:start 7/4 :end 2 :n 3}]
            (slice loop 0 2))
    (expect [{:start 1/2 :end 3/4 :n 2} {:start 3/4 :end 1 :n 3} {:start 1 :end 5/4 :n 0} {:start 5/4 :end 3/2 :n 1}]
            (slice loop 1/2 1/2 :offset? false))
    (expect [{:start 1/4 :end 1/2 :n 1}]
            (slice loop 1/8 1/2 :offset? false :mode :begin))
    (expect [{:start 0 :end 1/4 :n 0}]
            (slice loop 1/8 1/2 :offset? false :mode :end))
    (expect [{:start 0 :end 1/4 :n 0} {:start 1/4 :end 1/2 :n 1}]
            (slice loop 1/8 1/2 :offset? false :mode :active))))



(defexpect merge-loops-test
  (let [left  (p->l (p/fit 2 3))
        right (p->l (p/fit 4 5 6))
        mfn   +]
    (expect [{:start 0 :end 1/2 :n 6.0} {:start 1/2 :end 1 :n 8.0}]
            (->>
             (merge-> mfn left right)
             second
             (map #(select-keys % [:start :end :n]))))
    (expect [{:start 0 :end 1/3 :n 6.0} {:start 1/3 :end 2/3 :n 7.0} {:start 2/3 :end 1 :n 9.0}]
            (->>
             (<-merge mfn left right)
             second
             (map #(select-keys % [:start :end :n]))))
    (expect [{:start 0 :end 1/3 :n 6.0}
             {:start 1/3 :end 1/2 :n 7.0}
             {:start 1/2 :end 2/3 :n 8.0}
             {:start 2/3 :end 1 :n 9.0}]
            (->>
             (<merge> mfn left right)
             second
             (map #(select-keys % [:start :end :n]))))))
