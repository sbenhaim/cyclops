(ns ops-test
  (:require
   [shhh.ops :refer [assign-times apply-op ->Context]]
   [expectations.clojure.test :refer [defexpect expect]]))


(defexpect fit-op
  (expect [{:position 0 :duration 1/3 :period 1}
           {:position 1/3 :duration 1/3 :period 1}
           {:position 2/3 :duration 1/3 :period 1}]
          (assign-times [:fit {} {} {}]))

  (expect [{:position 1/2 :period 2 :duration 1/6}
           {:position 2/3 :period 2 :duration 1/6}
           {:position 5/6 :period 2 :duration 1/6}]
          (apply-op [:fit {} {} {}] (->Context 2 1/2 1 1/2)))

  (expect [{:position 0 :period 1 :duration 1/3}
           [{:position 1/3 :period 1 :duration 1/6} {:position 1/2 :period 1 :duration 1/6}]
           {:position 2/3 :period 1 :duration 1/3}]
          (assign-times [:fit {} [:fit {} {}] {}])))


(defexpect cycle-op
  (expect [{:position 0 :period 3 :duration 1}
           {:position 1 :period 3 :duration 1}
           {:position 2 :period 3 :duration 1}]
          (assign-times [:cycle {} {} {}]))

  (expect [{:position 0 :period 3 :duration 1}
           [{:position 1 :period 6 :duration 1} {:position 4 :period 6 :duration 1}]
           {:position 2 :period 3 :duration 1}]
          (assign-times [:cycle {} [:cycle {} {}] {}])))



(defexpect nest-cycle-and-fit
  (expect [{:position 0 :period 1 :duration 1/3}
           [{:position 1/3 :period 2 :duration 1/3}
            {:position 4/3 :period 2 :duration 1/3}]
           {:position 2/3 :period 1 :duration 1/3}]
          (assign-times [:fit {} [:cycle {} {}] {}]))

  (expect [[{:position 0 :period 3 :duration 1/2} {:position 1/2 :period 3 :duration 1/2}]
           {:position 1 :period 3 :duration 1}
           {:position 2 :period 3 :duration 1}]
          (assign-times [:cycle [:fit {} {}] {} {}]))

  (expect [[{:position 0 :period 1 :duration 1/9} {:position 1/9 :period 1 :duration 1/9} {:position 2/9 :period 1 :duration 1/9}]
           [{:position 1/3 :period 3 :duration 1/3} {:position 4/3 :period 3 :duration 1/3} {:position 7/3 :period 3 :duration 1/3}]
           {:position 2/3 :period 1 :duration 1/3}]
          (assign-times [:fit [:fit {} {} {}] [:cycle {} {} {}] {}]))

  (expect [{:position 0 :period 2 :duration 1}
           [{:position 1 :period 2 :duration 1/2}
            [{:position 3/2 :period 4 :duration 1/2} { :position 7/2 :period 4 :duration 1/2}]]]
          (assign-times [:cycle {} [:fit {} [:cycle {} {}]]]))

  (expect [{:position 0 :period 1 :duration 1/2}
           [{:position 1/2 :period 2 :duration 1/2}
            [{:position 3/2 :period 2 :duration 1/4} {:position 7/4 :period 2 :duration 1/4}]]]
          (assign-times [:fit {} [:cycle {} [:fit {} {}]]]))

  (expect [[{:position 0 :period 1 :duration 1/6} [{:position 1/6 :period 2 :duration 1/6} {:position 7/6 :period 2 :duration 1/6}]]
           [[{:position 1/3 :period 2 :duration 1/6} [{:position 1/2 :period 4 :duration 1/6} {:position 5/2 :period 4 :duration 1/6}]] {:position 4/3 :period 2 :duration 1/3}]
           {:position 2/3 :period 1 :duration 1/3}]
          (assign-times [:fit
                         [:fit {} [:cycle {} {}]]
                         [:cycle [:fit {} [:cycle {} {}]] {}]
                         {}])))



(defexpect repeat-op

  (expect (assign-times [:fit {} {}])
          (assign-times [:* 2 {}]))

  (expect (assign-times [:fit {} {} {} {}])
          (assign-times [:* 2 {} {}]))

  (expect (assign-times [:cycle [:fit {} {} {} {}] {} [:fit {} {}]])
          (assign-times [:cycle [:* 2 {} {}] {} [:* 2 {}]])))


(defexpect slow-op

  (expect [{:position 0 :period 2 :duration 2}]
          (assign-times [:/ 2 {}]))

  (expect (assign-times [:cycle {} {}])
          (assign-times [:/ 2 {} {}]))

  (expect (assign-times [:cycle [:fit {} {}] {}])
          (assign-times [:/ 2 [:fit {} {}] {}]))

  (expect (assign-times [:fit {} [:cycle {} {}] {}])
          (assign-times [:fit {} [:/ 2 {} {}] {}]))


  (expect [[{:position 0 :period 2 :duration 1}
            {:position 1 :period 2 :duration 1}]]
          (assign-times [:/ 2 [:fit {} {}]])))
