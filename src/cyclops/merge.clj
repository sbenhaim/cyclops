(ns cyclops.merge
  (:require [cyclops.events :as e]
            [cyclops.util :refer [arity]]))


(def unmerged #{:start :end :period})


(defn merge-with-fn
  [f]
  (fn [tc]
    (fn mrg [b a] ;; Args are switched since clojure merge goes opposite of uzu merge
      (cond

        (fn? a)
        ,,(case (arity a)
            0 (mrg b (a))
            1 (do (println "Warning: argumented fn in leftmost position. Passing `nil`")
                  (mrg b (a nil)))
            (mrg b (a nil tc)))

        (fn? b) (case (arity b)
                  0 (mrg a (b))
                  1 (b a)
                  (b a tc))

        (or (and (number? a) (number? b))
            (and (number? a) (nil? b))
            (and (nil? a) (number? b))) (f (or a 0) (or b 0))

        :else [a b]))))


(defn merge-left
  [with-fn]
  (fn [a b tc]
    (let [remerge (select-keys a unmerged)
          tc (assoc tc :start (:start a) :end (:end a))]
      (-> (merge-with (with-fn tc) b a) ;; Args are switched since clojure merge goes opposite of uzu merge
          (merge remerge)))))


(defn merge-both
  [with-fn]
  (fn [a b tc]
    (let [remerge (select-keys a unmerged)
          start (max (:start a) (:start b))
          end   (min (:end a) (:end b))
          tc (assoc tc :start start :end end)]
      (-> (merge-with (with-fn tc) b a) ;; Args are switched since clojure merge goes opposite of uzu merge
          (merge remerge)
          (assoc :start start :end end)))))



(defn merge-cycles
  [with-fn a b tc & {:keys [structure-from]
                     :or   {structure-from :left}}]
  (assert #{:left :both} structure-from)
  (let [[na nb]  (e/normalize-periods a b)
        period   (e/period na)
        tc       (merge {:from 0 :to period} tc)
        sa       (e/slice na tc {:mode :starts-during})
        merge-fn (case structure-from :left (merge-left with-fn) (merge-both with-fn))
        merged
        (reduce
         (fn [result e]
           (let [from    (:start e)
                 to      (:end e)
                 overlap (e/slice nb {:from from :to to} {:mode :active-during})
                 overlap (case structure-from :left (take 1 overlap) :both overlap)]

             (if (seq overlap)
               (concat result (mapv #(merge-fn e % tc) overlap))
               [e])))
         []
         sa)]
    merged))


(defrecord MergeGroup [merge-fn cycles structure-from]
  e/Cyclic
  (period [_] (apply e/lcp cycles))
  (events [this] (e/slice this (e/tc 0 (e/period this)) nil))
  (slice [this tc] (e/slice this tc {}))
  (slice [_this tc _opts]
    (if (= 1 (count cycles))
      (e/slice (first cycles) tc)
      (reduce (fn [a b] (merge-cycles merge-fn a b tc :structure-from structure-from))
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
