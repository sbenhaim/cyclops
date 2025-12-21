(ns cycl.merge
  (:require [cycl.event :as e]
            [cycl.util :as u]
            [cycl.val :as v]
            [cycl.cycl :as c]))


(defn left-merge
  "First value wins"
  [a _b] a)


(defrecord FnMerge [mfn a b]
  v/DoYouRealize?
  (realize [_ ctx]
    (v/realize
     (mfn (v/realize a ctx)
          (v/realize b ctx))
     ctx)))


(defn fn-merge
  "Workhorse"
  [f]
  (fn [a b]
    (->FnMerge f a b)))


(def stack-merge
  "Combine as vector, i.e. played simultaneously"
  (fn-merge (fn [a b] (u/set* a b))))


(def or-merge
  "First truthy value wins."
  (fn-merge #(or %1 %2)))


(defn apply-merge
  "If b is fn, apply to a"
  [a b]
  (v/->Realize+Apply b a))


(defn apply|fn-merge
  "If b is fn, apply to a. Otherwise apply fn `f` to realized values of a and b"
  [f]
  (fn [a b]
    (if (fn? b)
      (->ApplyMerge a b)
      (->FnMerge f a b))))


(def apply|left-merge
  (apply|fn-merge left-merge))


(def apply|stack-merge
  (apply|fn-merge stack-merge))


(defn apply|maths|or|stack-merge
  "Complex but arguably intuitive combination of merge behavior where:
  1. If b is fn, apply to a
  2. If both a and b (left/right) realize to numerics--or nil/false treated as 0--pass as args to provided numeric function `f` (like `+` or `*`)
  3. If one of a or b is nil/false, choose the other
  4. Stack them
"
  [f]
  (apply|fn-merge
   (fn [a b]
     (cond
       (u/num-enough? a b) (f (or a 0) (or b 0))
       (and a b) (u/set* a b)
       :else (or a b)))))


(defn apply|maths|or|left-merge
  [f]
  (apply|fn-merge
   (fn [a b]
     (cond
       (u/num-enough? a b) (f (or a 0) (or b 0))
       (and a b) a
       :else (or a b)))))



(defn merge-events-left
  "Logic for structure-from-left merge.
  Do not pass directly to merge fns. Used for internal logic."
  [with-fn]
  (fn [a [b & _]]
    [(update a :params
             #(merge-with
               (fn [b a] (with-fn a b))
               (:params b) %))]))


(defn merge-events-split
    "Logic for structure-from-both merge.
  Do not pass directly to merge fns. Used for internal logic."
  [with-fn]
  (fn [a bs]
    (map
     (fn [b]
       (let [start      (max (:start a) (:start b))
             full-start (max (e/start a) (e/start b))
             end        (min (e/end a) (e/end b))
             [merged]   ((merge-events-left with-fn) a [b])]
         (assoc merged :start start :length (- end full-start))))
     bs)))



(defn merge-cycles
  [merge-fn cycl-a cycl-b]
  (mapcat
   (fn [a]
     (let [overlap (c/slice-active cycl-b (e/start a) (e/length a))]
       (if (seq overlap)
         (merge-fn a overlap)
         [a])))
   cycl-a))
