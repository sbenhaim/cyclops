(ns cyclops.merge
  (:require [cyclops.events :as e]
            [cyclops.util :as u]))


(defn left-merge
  "First value wins (i.e., a, usually)"
  [_b a]
  a)


(defn fn-merge
  "Workhorse"
  [f]
  (fn [b a]
    (fn [ctx]
      (f (e/realize a ctx)
         (e/realize b ctx)))))


(def stack-merge
  "Combine as vector, i.e. played simultaneously"
  (fn-merge vector))


(def or-merge
  "First truthy value wins."
  (fn-merge #(or %1 %2)))


(defn apply-merge
  "If b is fn, apply to a"
  [b a]
  (fn [ctx]
    (cond
      (u/fn1? b) (b (e/realize a ctx))
      (u/fn2? b) (b (e/realize a ctx) ctx))))


(defn apply|fn-merge
  "If b is fn, apply to a. Otherwise apply fn `f` to realized values of a and b"
  [f]
  (fn [b a]
    (fn [ctx]
      (cond
        (u/fn1? b) (b (e/realize a ctx))
        (u/fnv? b) (b (e/realize a ctx))
        (u/fn2? b) (b (e/realize a ctx) ctx)
        :else (f (e/realize a ctx) (e/realize b ctx))))))


(def apply|left-merge
  (apply|fn-merge left-merge))


(def apply|stack-merge
  (apply|fn-merge vector))


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
       (and a b) (vector a b)
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
  (fn [a b]
    (update a :params
            #(merge-with with-fn (:params b) %))))


(defn merge-events-both
    "Logic for structure-from-both merge.
  Do not pass directly to merge fns. Used for internal logic."
  [with-fn]
  (fn [a b]
    (let [start (max (:start a) (:start b))
          end   (min (e/end a) (e/end b))]
      (-> ((merge-events-left with-fn) a b)
          (assoc :start start :length (- end start))))))



(defn merge-cycles
  "TODO: How to better deail with all the case statements?"
  [merge-fn a b mode]
  (assert #{:left-merge :double-merge :op-merge} mode)
  (let [[na nb]    (e/normalize-periods a b)
        new-period (e/period na)
        slice-mode (case mode
                     :double-merge :active-during  ;; Double merges any events that overlap
                     :left-merge   :starts-during  ;; Left merge merges a b that *starts* during a
                     :op-merge     :active-during) ;; Ditto op-merge
        merged
        (reduce
         (fn [result e]
           (let [overlap (e/slice nb (:start e) (:length e) slice-mode)
                 overlap (case mode
                           :double-merge overlap           ;; Double merge events can multiply
                           :left-merge  (take 1 overlap)   ;; While in left merge, one a event becomes one merged event
                           :op-merge    overlap            ;; Op merge tbd
                           overlap)]
             (if (seq overlap)
               (case mode
                 :double-merge (concat result (mapv #(merge-fn e %) overlap)) ;; Double merge events multiply
                 (conj result (merge-fn e overlap)))
               (case mode
                 :op-merge result ;; Op merge doesn't act if there is no overlap
                 (conj result [e]))))) ;; Other merges keep the event unchanged
         []
         (e/events na))]
    (e/->Cycle new-period merged)))



(defn merge-cycles*
  ([f cycs] (merge-cycles* f cycs :double-merge))
  ([f cycs mode]
   (let [merge-fn (case mode :left-merge (merge-events-left f) (merge-events-both f))]
     (reduce (fn [a b] (merge-cycles merge-fn a b mode)) cycs))))
