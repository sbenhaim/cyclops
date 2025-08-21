(ns cyclops.looping
  (:require
   [clojure.math.numeric-tower :refer [lcm]]
   [cyclops.pattern :refer [process-pattern]]))


;; Loop Maths
;;
(defn cycle-loop
  "Like clojure.core/cycle but, moves events cyclically forward in time as it cycles"
  ([[loop-period evts]] (cycle-loop nil [loop-period evts]))
  ([n [loop-period evts]]
   (let [loop (->> evts
                   repeat
                   (mapcat
                    (fn [i cycle]
                      (for [e cycle]
                        (-> e
                            (update :start #(+ % (* i loop-period)))
                            (update :end #(+ % (* i loop-period))))))
                    (range)))]
     (if n
       (take (* n (count evts)) loop)
       loop))))




(defn lcm-loops
  "Combines loops of different period into a single loop of `lcm` period"
  [[period-a loop-a] [period-b loop-b]]
  (let [c-period (lcm period-a period-b)]
    [[c-period (cycle-loop (/ c-period period-a) [period-a loop-a])]
     [c-period (cycle-loop (/ c-period period-b) [period-b loop-b])]]))


(defn sync-periods
    [ragged]
    (let [loops  (->> ragged
                      (group-by :period)
                      (reduce lcm-loops))
          period (ffirst loops)
          events (apply concat (map second loops))]
      [period (sort evt-sort events)]))


(defn slice
  "Given values `from` and `to` representing cycle-relative starts (i.e, produced by `s->pos`
  returns a slice of a loop falling within the two points in time.

  If `to` is > `loop-period`, the loop is (lazily) cycled to the length needed.

  When `offset?` is true, cycle-relative times are offset by `from` to produce start-relative times for scheduling."
  [[period loop] from to & {:keys [offset? mode]
                            :or   {offset? false
                                   mode    :begin}}]
  (let [to   (if (<= to from) (+ to period) to)
        loop (if (> to period) (cycle-loop [period loop]) loop)
        [key1 key2 compr] (case mode
                            :begin  [:start :start >]
                            :end    [:end :end >]
                            :active [:end :start >=])
        slc  (into []
                   (comp
                    (drop-while #(compr from (key1 %)))
                    (take-while #(> to (key2 %))))
                   loop)]
    (if offset?
      (->> slc
           (mapv (fn [e] (update e :start #(- % from))))
           (mapv (fn [e] (update e :end #(- % from)))))

      slc)))


(defn merge-left
  [afn a b]
  ;; (println a b)
  (-> (merge b a)
      (assoc :n (afn (get a :n 0) (get b :n 0)))))


(defn merge-both
  [afn a b]
  (let [a (-> a
              (assoc :start (max (:start a) (:start b)))
              (assoc :end (min (:end a) (:end b))))]
    (merge-left afn a b)))


(defn merge-loops
  [with-fn a b & {:keys [structure-from]
                  :or   {structure-from :left}}]
  (let [[a [period b]] (lcm-loops a b)
        slice-mode     (case structure-from :left :begin :both :active)
        merged
        (reduce
         (fn [result e]
           (let [from    (:start e)
                 to      (+ from (:length e))
                 overlap (slice a from to :mode slice-mode :offset? false)]
             (concat result (mapv #(with-fn % e) overlap))))
         []
         b)]
    [period merged]))


(defn merge-> [merge-fn & loops]
  (let [afn (partial merge-left merge-fn)]
    (reduce (partial merge-loops afn) loops)))


(defn <-merge [merge-fn & loops]
  (let [afn (partial merge-left merge-fn)]
    (reduce #(merge-loops afn %2 %1) loops)))

(defn <merge> [merge-fn & loops]
  (let [afn (partial merge-both merge-fn)]
    (reduce #(merge-loops afn %1 %2 :structure-from :both) loops)))


(defn |+ [& loops]
  (apply merge-> + loops))


(defn +| [& loops]
  (apply <-merge + loops))


(defn |+| [& loops]
  (apply <merge> + loops))


(defn periodic-loops
  "After assigning event times to a pattern, converts nested loops of various period
  into a hashmap of {loop-period loop}."
  [flat]
  (->> flat
       (group-by :period)
       (into [])))


(defn pattern->loops
  [pattern]
  (-> pattern process-pattern periodic-loops))

(comment
  (let [[a] (pattern->loops [:saw :tri :square])
        [b] (pattern->loops [2 3])
        [c] (pattern->loops [4 5 6])
        [_ merged] (|+| c b)]
    (mapv #(select-keys % [:n :s]) merged)))
