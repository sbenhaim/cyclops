(ns cyclops.merge
  (:require [cyclops.events :as e :refer [realize-val]]))


(def unmerged #{:start :end :period})


(defn merge-with-fn
  [f]
  (fn mrg [b a] ;; Args are switched since clojure merge goes opposite of uzu merge
    (fn [tc]
      (let [a (realize-val tc a)
            b (realize-val tc b)]
        (cond

          (fn? b) (b a tc)

          (or (and (number? a) (number? b))
              (and (number? a) (nil? b))
              (and (nil? a) (number? b))) (f (or a 0) (or b 0))

          :else [a b])))))



(defn merge-left
  [with-fn]
  (fn [a b]
    (let [remerge (select-keys a unmerged)]
      (-> (merge-with with-fn b a) ;; Args are switched since clojure merge goes opposite of uzu merge
          (merge remerge)))))


(defn merge-both
  [with-fn]
  (fn [a b]
    (let [remerge (select-keys a unmerged)
          start (max (:start a) (:start b))
          end   (min (:end a) (:end b))]
      (-> (merge-with with-fn b a) ;; Args are switched since clojure merge goes opposite of uzu merge
          (merge remerge)
          (assoc :start start :end end)))))



(defn merge-cycles
  [with-fn a b from to & {:keys [structure-from realize?]
                          :or   {structure-from :left
                                 realize? false}}]
  (assert #{:left :both} structure-from)
  (let [[na nb]  (e/normalize-periods a b)
        sa       (e/slice na from to {:mode :starts-during})
        merge-fn (case structure-from :left (merge-left with-fn) (merge-both with-fn))
        merged
        (reduce
         (fn [result e]
           (let [from    (:start e)
                 to      (:end e)
                 overlap (e/slice nb from to {:mode :active-during})
                 overlap (case structure-from :left (take 1 overlap) :both overlap)]

             (if (seq overlap)
               (concat result (mapv #(merge-fn e %) overlap))
               [e])))
         []
         sa)]
    (if realize?
      (map e/realize merged)
      merged)))


(defrecord MergeGroup [merge-fn cycles structure-from]
  e/Cyclic
  (period [_] (apply e/lcp cycles))
  (events [this] (e/slice this 0 (e/period this) {:realize? true}))
  (slice [this from to] (e/slice this from to {}))
  (slice [_this from to opts]
    (if (= 1 (count cycles))
      (e/slice (first cycles) from to opts)
      (reduce (fn [a b] (apply merge-cycles merge-fn a b from to :structure-from structure-from opts))
              cycles))))

(comment
  (require '[cyclops.control :refer [n* s* pan]])
  (let [a (n* 0 2 4 5)
        b (s* :supermandolin)
        c (pan [0 0.5 1])
        f (merge-with-fn +)]

    #_(merge-cycles f a b (e/tc 0 1) :structure-from :left)

    (e/events
       (->MergeGroup f [a b] :both))))



(defn |+ [& cycles]
  (->MergeGroup (merge-with-fn +) cycles :left))


(defn +| [& cycles]
  (->MergeGroup (merge-with-fn +) (reverse cycles) :left))


(defn |+| [& cycles]
  (->MergeGroup (merge-with-fn +) cycles :both))
