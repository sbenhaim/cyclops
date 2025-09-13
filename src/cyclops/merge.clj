(ns cyclops.merge
  (:require [cyclops.events :as e]
            [cyclops.util :as u]))


(defn left-merge [_b a]
  a)


(defn fn-merge [f]
  (fn [b a]
    (fn [ctx]
      (f (e/realize a ctx)
         (e/realize b ctx)))))


(def stack-merge (fn-merge vector))


(defn apply-merge
  [b a]
  (fn [ctx]
    (cond
      (u/fn1? b) (b (e/realize a ctx))
      (u/fn2? b) (b (e/realize a ctx) ctx))))


(defn apply|fn-merge
  [f]
  (fn [b a]
    (fn [ctx]
      (cond
        (u/fn1? b) (b (e/realize a ctx))
        (u/fn2? b) (b (e/realize a ctx) ctx)
        :else (f (e/realize a ctx) (e/realize b ctx))))))


(def apply|left-merge
  (apply|fn-merge (fn [a _b] a)))


(def apply|stack-merge
  (apply|fn-merge vector))


(defn apply|maths|stack-merge
  [f]
  (apply|fn-merge
   (fn [a b]
     (if (u/num-enough? a b) (f a b) (vector a b)))))


(defn merge-left
  [with-fn]
  (fn [a b]
    (update a :params
            #(merge-with with-fn (:params b) %))))


(defn merge-both
  [with-fn]
  (fn [a b]
    (let [start (max (:start a) (:start b))
          end   (min (e/end a) (e/end b))]
      (-> ((merge-left with-fn) a b)
          (assoc :start start :length (- end start))))))


(defn merge-two
  [merge-fn a b & {:keys [structure-from]
                   :or   {structure-from :both}}]
  (assert #{:left :both} structure-from)
  (let [[na nb]  (e/normalize-periods a b)
        new-period (e/period na)
        merged-cxfs (into [] (set (concat (e/cycle-xfs a) (e/cycle-xfs b))))
        merged-pxfs (merge-with u/vector* (e/param-xfs a) (e/param-xfs b))
        merged
        (reduce
         (fn [result e]
           (let [overlap (e/slice nb (:start e) (:length e) {:mode :active-during})
                 overlap (case structure-from :left (take 1 overlap) :both overlap)]
             (if (seq overlap)
               (concat result (mapv #(merge-fn e %) overlap))
               [e])))
         []
         (e/events na))]
    (e/->Cycle new-period merged merged-cxfs merged-pxfs)))


(defn merge-cycles
  ([f cycs] (merge-cycles f cycs :both))
  ([f cycs structure-from]
   (let [merge-fn (case structure-from :left (merge-left f) (merge-both f))]
     (reduce (fn [a b] (merge-two merge-fn a b {:structure-from structure-from})) cycs))))
