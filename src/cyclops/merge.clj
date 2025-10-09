(ns cyclops.merge
  (:require [cyclops.events :as e]
            [cyclops.util :as u]))


(defn left-merge
  "First value wins (i.e., a, usually)"
  [a _b] a)


(defn fn-merge
  "Workhorse"
  [f]
  (fn [a b]
    (fn [_ ctx]
      (f (e/realize a ctx)
         (e/realize b ctx)))))


(def stack-merge
  "Combine as vector, i.e. played simultaneously"
  (fn-merge (fn [a b] (u/vector* a b))))

(def or-merge
  "First truthy value wins."
  (fn-merge #(or %1 %2)))


(defn apply-merge
  "If b is fn, apply to a"
  [a b]
  (fn [_ ctx]
    (cond
      (u/fn1? b) (b (e/realize a ctx))
      (u/fn2? b) (b (e/realize a ctx) ctx))))


;; TODO: The result of some of these merges is a fn that should be realized and provided to f
;; But if that fn takes 1 2 or v args, we apply instead.
;; And it needs to accept args to accept ctx
;; What is the way to signal that we don't want to apply, but fall
;; back to the fn
(defn apply|fn-merge
  "If b is fn, apply to a. Otherwise apply fn `f` to realized values of a and b"
  [f]
  (fn a|fm1 [a b]
    (fn a|fm2 [_ ctx]
      (cond
        (u/fn1? b) (b (e/realize a ctx))
        (u/fnv? b) (b (e/realize a ctx))
        ;; (u/fn2? b) (b (e/realize a ctx) ctx) ;; <- Seems to work
        :else (f (e/realize a ctx) (e/realize b ctx))))))


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
            #(merge-with
              (fn [b a] (with-fn a b))
              (:params b) %))))


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
  (let [[na nb]    (e/normalize-periods [a b])
        slice-mode (case mode
                     :double-merge :active-during  ;; Double merges any events that overlap
                     :left-merge   :starts-during  ;; Left merge merges a b that *starts* during a
                     :op-merge     :active-during) ;; Ditto op-merge
        ]
    (reduce
     (fn [result e]
       (let [overlap (e/slice nb (:start e) (:length e) slice-mode)
             overlap (case mode
                       :double-merge overlap           ;; Double merge events can multiply
                       :left-merge   (take 1 overlap)   ;; While in left merge, one a event becomes one merged event
                       :op-merge     overlap            ;; Op merge tbd
                       overlap)]
         (if (seq overlap)
           (case mode
             :double-merge (concat result (mapv #(merge-fn e %) overlap)) ;; Double merge events multiply
             (conj result (merge-fn e overlap)))
           (case mode
             :op-merge result ;; Op merge doesn't act if there is no overlap
             (conj result [e]))))) ;; Other merges keep the event unchanged
     []
     na)))



(defn merge-cycles*
  ([f cycs] (merge-cycles* f cycs :double-merge))
  ([f cycs mode]
   (let [merge-fn (case mode :left-merge (merge-events-left f) (merge-events-both f))]
     (reduce (fn [a b] (merge-cycles merge-fn a b mode)) cycs))))
