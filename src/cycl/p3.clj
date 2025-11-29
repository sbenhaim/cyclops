(ns cycl.p3
  (:require [cycl.event :as e]
            [cycl.util :as u]))


(defn ->event
  ([v] (->event v 0 1))
  ([v start length]
   (let [evt {:start start :length length :period 1}]
     (if (map? v)
       (assoc evt :params v)
       (assoc evt :params {:init v})))))


(defprotocol Op
  (operate [this])
  (period [this])
  (weight [this]))


(defn weigh
  [pattern]
  (reduce + (map weight pattern)))


(defn slice [evts from length]
  (->> evts
       (drop-while #(< (e/start %) from))
       (take-while #(< (e/start %) (+ from length)))))


(defn realize
  ([pat] (realize pat 1))
  ([pat length] (realize pat 0 length))
  ([pat from length]
   (slice (operate pat) from length)))


(defrecord Gen [value]
  Op
  (operate [_]
    (map #(->event value % 1) (range)))
  (period [_] 1)
  (weight [_] 1))


(comment
  (let [g (->Gen :a)]
    (realize g 1/2 5/2)))


(defn iter
  [offset n period]
  (mod (quot offset n) period))


(comment
  (iter 1 3 2)
  (iter 3 3 2)
  (iter 5 3 2)
  (iter 7 3 3))



(defn arrange
  [tx-evt pats]
  (let [weights (map weight pats)
        n       (reduce + weights)
        weights* (cycle weights)
        offsets (reductions + 0 weights*)]
    (-> (mapcat
         (fn [pat weight offset]
           (let [sub-p (period pat)
                 iter-no (iter offset n sub-p)
                 sub-pat (slice (operate pat) iter-no 1)]
             (for [evt sub-pat]
               (let [[iter-no frac] (u/mixed (e/start evt))]
                 (tx-evt evt offset weight iter-no frac n)))))
         (cycle pats)
         weights*
         offsets))))


(defrecord FitOp [patterns]
  Op
  (operate [_]
    (arrange
     (fn [evt offset weight _iter-no frac n]
       (let [scale (/ n)]
         (-> evt
             (assoc :start (+ (* offset scale) (* frac scale weight)))
             (update :length #(* % scale weight)))))
     patterns))
  (period [_] 1)
  (weight [_] 1))


(comment
  (slice (operate (->FitOp (map ->pat [:a :b :c]))) 0 2))



(defrecord CyclOp [patterns]
  Op
  (operate [_]
    (arrange
     (fn [evt offset weight _iter-no frac n]
       (-> evt
           (assoc :start (+ offset frac))
           (update :length #(* % weight))
           (update :period #(* % n))))
     patterns))
  (period [_] (let [ps    (map period patterns)
                    top-p (weigh patterns)]
                (* top-p (apply u/lcm ps))))
  (weight [_] 1))


(comment
  (let [
        ;; f (->CyclOp (map ->pat [:a (->FitOp (map ->pat [:b :c]))]))
        f (->CyclOp (map ->pat [:a (->CyclOp (map ->pat [:b :c]))]))
        ;; f (->CyclOp (map ->pat [:a :b]))
        ]
    #_(period (->CyclOp (map ->pat [:c :d])))
    (slice (operate f) 0 4)
    #_(period f)))


(defrecord TimesOp [n pat]
  Op
  (operate [_]
    (map
     (fn [e]
       (-> e
           (update :start #(* % (/ n)))
           (update :length #(* % (/ n)))))
     (operate (->FitOp pat))))
  (period [_] 1)
  (weight [_] 1))


(comment
  (realize
   (->TimesOp 4 (map ->pat [:a]))))


(defrecord ElongateOp [x pat]
  Op
  (operate [_] (operate (->FitOp pat)))
  (period [_] 1)
  (weight [_] (* x (weight pat))))

(comment
  (realize
   (->FitOp (map ->pat [:a (->ElongateOp 2 (map ->pat [:b]))]))))


(defrecord RepeatOp [n pat]
  Op
  (operate [_] (operate (->TimesOp n pat)))
  (period [_] 1)
  (weight [_] (* n (weight pat))))


(defn ->pat [x]
  (cond
    (satisfies? Op x) x
    (sequential? x)   (->FitOp (map ->pat x))
    :else             (->Gen x)))




;; ops


(defn fit [& pat]
  (->FitOp (map ->pat pat)))

(defn cyc [& pat]
  (->CyclOp (map ->pat pat)))


(comment
  (realize (fit :a :b :c))
  (realize (fit :a (fit :b :c)))
  (realize (cyc :a :b :c) 6)
  (realize (cyc :a (cyc :b :c)) 4)
  (realize (cyc :a (fit :b :c)) 2)
  (realize (fit :a (cyc :b :c)) 2)
  )
