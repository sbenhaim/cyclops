(ns cyclops.merge
  (:require [cyclops.events :as e]
            [cyclops.util :as u]))


(defn stack-merge [b a]
  (e/qxf a #(u/vector* a b)))



(defn apply|fn-merge
  [f]
  (fn [b a]
    (e/qxf a (if (and (fn? b) (not= 0 (u/arity b))) b ;; apply
                 #(f (assoc % :cur (e/realize-val b %))))))) ;; fn


(def apply|left-merge
  (apply|fn-merge #(:prev %)))


(def apply|stack-merge
  (apply|fn-merge #(u/vector* (:prev %) (:cur %))))



(defn apply|maths|stack-merge ;; TODO: Can we do this through composition
  [f]
  (apply|fn-merge
   (fn [{:keys [prev cur]}]
     (if (u/num-enough? prev cur) (f prev cur) (u/vector* prev cur)))))


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
  [with-fn a b & {:keys [structure-from]
                  :or   {structure-from :both}}]
  (assert #{:left :both} structure-from)
  (let [[na nb]  (e/normalize-periods a b)
        new-period (e/period na)
        merged-cxfs (into [] (set (concat (e/cycle-xfs a) (e/cycle-xfs b))))
        merged-pxfs (merge-with u/vector* (e/param-xfs a) (e/param-xfs b))
        merge-fn (case structure-from :left (merge-left with-fn) (merge-both with-fn))
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
   (reduce (fn [a b] (merge-two f a b {:structure-from structure-from})) cycs)))
