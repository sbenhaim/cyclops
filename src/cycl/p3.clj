(ns cycl.p3
  (:require [cycl.event :as e]
            [cycl.cycl :as c]
            [cycl.util :as u]))


(defn ->cycl?
  [thing]
  (cond
    (c/cycl? thing)  thing
    (e/event? thing) [thing]
    :else            [(e/->event thing)]))


(defn encyclify
  [pattern]
  (map ->cycl? pattern))


(defn weigh
  [cycl]
  (or (-> cycl meta :weight) 1))


(defn weigh*
  [cycls]
  (reduce + (map weigh cycls)))


(comment
  (e/translate (fit :a :b :c) 1/2 1/2 1)
  (-> (fit :a :b :c) (e/translate 1/2 1 1) (e/zero))
  (-> (fit :a :b :c) (e/translate 1/2 1/2 1) (e/translate))

  (-> (cyc :a :b (cyc :c :d)) e/normalize-cycl (e/translate 0 6 6))
  )


(defn arrange
  [f cycls]
  (let [weights (map weigh cycls)
        n       (reduce + weights)
        offsets (reductions + 0 weights)]
    (mapcat (fn [cycl weight offset]
              (f cycl weight offset n))
            cycls weights offsets)))


(defn scale
  ([x cycls] (scale x cycls false))
  ([x cycls cyclic?]
   (let [weights (map weigh cycls)
         n       (reduce + weights)
         offsets (reductions + 0 weights)
         factor  (/ x n)]
     (->
      (mapcat (fn [cycl weight offset]
                (map
                 (fn [evt]
                   (let [start     (:start evt)
                         cycle     (long start)
                         frac      (- start cycle)
                         new-cycle (if cyclic? (* n cycle) cycle)]
                     (-> evt
                         (assoc :start (+ new-cycle (* offset factor) (* frac weight factor)))
                         (update :length #(* % weight factor))
                         (update :period #(* % x)))))
                 cycl))
              cycls weights offsets)
      (c/normalize)))))


(comment
  (scale 1 (encyclify [:a :b :c :d]))
  (scale 1 (encyclify [:a :b (scale 1 (encyclify [:c :d]))]))
  (scale 1 (encyclify [:a :b (with-meta (scale 1 (encyclify [:c :d])) {:weight 2})]))
  (scale 2 (encyclify [:a :b (with-meta (scale 1 (encyclify [:c :d])) {:weight 2})]))
  (scale 1/2 (encyclify [:a :b (scale 1 (encyclify [:c :d]))])))


(comment
  (scale 4 (encyclify [:a :b :c :d]) true)
  (scale 3 (encyclify [:a :b :c :d]) true)
  (scale 2 (encyclify [:a :b :c :d]) true)
  (scale 3 (encyclify [:a :b (scale 2 (encyclify [:c :d]))]) true)
  (scale 2 (encyclify [:a :b (scale 2 (encyclify [:c :d]))]) true)
  (scale 4 (encyclify [:a :b (with-meta (scale 1 (encyclify [:c :d])) {:weight 2})]) true)
  (scale 2 (encyclify [:a :b (with-meta (scale 1 (encyclify [:c :d])) {:weight 2})]) true)
  (scale 1/2 (encyclify [:a :b (scale 1 (encyclify [:c :d]))]) true))


;; Ops


(defn fit
  [& pattern]
  (let [cycls (encyclify pattern)]
    (scale 1 cycls)))

(comment
  (fit :a :b :c)
  (fit :a (fit :b :c)))


(defn cyc
  [& pattern]
  (let [cycls (encyclify pattern)
        n (reduce + (map weigh cycls))]
    (scale n cycls true)))


(comment
  (cyc :a :b :c)
  (cyc :a (cyc :b :c)))


(comment
  (fit :a (cyc :b :c))
  (cyc :a (fit :b :c))
  (fit (cyc :a :b) (cyc :c (fit :d :e)) :f)
  (cyc (fit :a :b) (cyc :c (fit :d :e)) :f))


(defn times-op
  [n cycl]
  (let [[start length period] (c/shape cycl)]
    (-> (map #(assoc % :period length) cycl)
        (->> (c/loop-cycl n))
        (c/translate start length period))))


(comment
  (times-op 2 (fit :a))
  (fit :a (times-op 2 (fit :b)))
  (times-op 2 [{:start 1/4 :length 1/4 :period 1}])
  (times-op 4 [{:start 1/2 :length 1/2 :period 1}]))

(comment
  (times-op 2 [{:start 0 :length 1 :period 2}])
  (times-op 1 [{:start 0 :length 1/2 :period 2}
                {:start 1/2 :length 1/2 :period 2}])
  (times-op 1 [{:params {:init :a}, :start 0, :length 1/2, :period 2}
               {:params {:init :b}, :start 1/2, :length 1/2, :period 2}]))


(defn elongate-op
  [x cycl]
  (with-meta cycl {:weight x}))


(comment
  (fit (elongate-op 2 (fit :a)) :b)
  (fit (elongate-op 2 (fit :a :b)) :c)
  (cyc (elongate-op 2 (fit :a)) :c))


(defn repeat-op
  [n cycl]
  (elongate-op n (times-op n cycl)))


(comment
  (fit (repeat-op 2 (fit :a)) :b)
  (fit (times-op 2 (fit :a :b)) :c)
  (fit (repeat-op 2 (fit :a :b)) :c))


(defn by-iter
  [cycls]
  (group-by (fn [c] (-> c first e/iter)) cycls))


(defn reweight
  [cycls]
  (mapcat
   (fn [[iter cycls]]
     (let [lengths (map c/length cycls)
           weights (map weigh cycls)
           weighted-lengths (u/weighted lengths weights)
           starts (reductions + 0 weighted-lengths)]
       (mapcat
        (fn [c s l]
          (c/translate c (+ s iter) l (c/period c)))
        cycls
        starts
        weighted-lengths)))
   (by-iter cycls)))


(defn op-merge
  [op1 arg-cycl evt-cycl]
  (let [[arg-cycl evt-cycl] (c/normalize-periods [arg-cycl evt-cycl])
        cycls
        (map
         (fn [arg-evt]
           (let [arg     (e/get-init arg-evt)
                 overlap (c/slice evt-cycl (e/start arg-evt) (e/length arg-evt) :active-during)]
             (op1 arg overlap)))
         arg-cycl)]
    (reweight cycls)))


(comment
  (op-merge times-op (fit 1 2) (fit :a :b))
  (op-merge times-op (cyc 1 2) (fit :a :b))

  (op-merge times-op (fit (el 2 1) 2) (fit :a :b :c))
  (op-merge times-op (fit (el 2 2) 1) (fit :a :b :c))

  (op-merge elongate-op (fit 2 1) (fit :a :b))
  (op-merge elongate-op (fit 1 (cyc 1 2 3)) (fit :a :b))


  (op-merge times-op (fit 2 (el 2 1)) (fit :a (el 2 :b)))
  (op-merge times-op (cyc (fit 2 1) (fit 1 2)) (fit :a :b))

  (op-merge elongate-op (cyc (fit 2 1) (fit 1 3)) (fit :a :b))

  (op-merge elongate-op (fit 1 (cyc 1 2 3)) (fit :a :b))

  (op-merge times-op (cyc 1 2) (cyc :a :b)))




(defn x
  [n & pattern]
  (times-op n (apply fit pattern)))


(defn el
  [x & pattern]
  (elongate-op x (apply fit pattern)))


(comment
  (fit :a (spl :b :c))
  (cyc :a (spl :b :c))
  (fit :a (spl :b (el 2 :c)))
  (cyc :a (spl :b (el 2 :c))))


(comment
  (fit :a (el 2 :b))
  (fit :a (fit :b (el 2 :c)))
  (cyc :a (el 2 :b))
  (cyc :a (cyc :b (el 2 :c))))




(defn rep
  [n & pattern]
  (repeat-op n (apply fit pattern)))


(comment
  (fit :a (rep 2 :b))
  (cyc :a (rep 2 :b)))

(defn spl
  [& pattern]
  (let [cycls (encyclify pattern)
        weight (reduce + (map weigh cycls))]
    (apply el weight pattern)))

