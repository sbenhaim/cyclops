(ns cycl.p3
  (:require [cycl.events :as e]
            [cycl.util :as u]
            [cycl.merge :as merge]))


(defn ->cycl?
  [thing]
  (cond
    (e/cycl? thing)  thing
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


(defn scale
  ([x cycls] (scale x cycls false))
  ([x cycls cyclic?]
   (let [weights (map weigh cycls)
         n       (reduce + weights)
         offsets (reductions + 0 weights)
         factor  (/ x n)]
     (mapcat (fn [cycl weight offset]
               (map
                (fn [evt]
                  (let [start (:start evt)
                        cycle (long start)
                        frac  (- start cycle)
                        new-cycle (if cyclic? (* n cycle) cycle)]
                    (-> evt
                        (assoc :start (+ new-cycle (* offset factor) (* frac weight factor)))
                        (update :length #(* % weight factor))
                        (update :period #(* % x)))))
                cycl))
             cycls weights offsets))))


(comment
  (scale 1 (encyclify [:a :b :c :d]) false)
  (scale 1 (encyclify [:a :b (scale 1 (encyclify [:c :d]) false)]) false)
  (scale 1 (encyclify [:a :b (with-meta (scale 1 (encyclify [:c :d]) false) {:weight 2})]) false)
  (scale 2 (encyclify [:a :b (with-meta (scale 1 (encyclify [:c :d]) false) {:weight 2})]) false)
  (scale 1/2 (encyclify [:a :b (scale 1 (encyclify [:c :d]) false)]) false))


(comment
  (scale 4 (encyclify [:a :b :c :d]))
  (scale 3 (encyclify [:a :b :c :d]))
  (scale 2 (encyclify [:a :b :c :d]))
  (scale 3 (encyclify [:a :b (scale 2 (encyclify [:c :d]))]) true)
  (scale 2 (encyclify [:a :b (scale 2 (encyclify [:c :d]))]) true)
  (scale 4 (encyclify [:a :b (with-meta (scale 1 (encyclify [:c :d])) {:weight 2})]))
  (scale 2 (encyclify [:a :b (with-meta (scale 1 (encyclify [:c :d])) {:weight 2})]))
  (scale 1/2 (encyclify [:a :b (scale 1 (encyclify [:c :d]))])))


;; Ops


(defn fit
  [& pattern]
  (let [cycls (encyclify pattern)]
    (scale 1 cycls false)))

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
  (scale 1 (repeat n cycl)))


(comment
  (times-op 2 (fit :a))
  (fit :a (times-op 2 (fit :b))))


(defn x
  [n & pattern]
  (times-op n (apply fit pattern)))


(defn elongate-op
  [x cycl]
  (with-meta cycl {:weight x}))


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


(defn repeat-op
  [n cycl]
  (elongate-op n (times-op n cycl)))



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
