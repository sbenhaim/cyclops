(ns cyclops.pattern
  (:require
   [clojure.math.numeric-tower :refer [lcm]]
   [clojure.pprint :refer [pprint]]))


(defn ->event
  "Expands shorthand notation to an (untimed) event map."
  [sh]
  (cond
    (coll? sh) sh
    :else {:value sh}))


(defn ->events
  "See `cyclops.pattern/->event`"
  [shs]
  (map ->event shs))


(defrecord Context
    [period         ;; Number of cycles before looping
     segment-length ;; How much segments does it occupy per cycle
     spacing        ;; How many segments between events
     start          ;; Where it sits in the pattern
     ])


(defprotocol Op
  (weight [this])
  (apply-op [this context])
  (display [this]))


(defn op? [e] (satisfies? Op e))


(defn event? [e] (not (op? e)))


(defn weigh [e]
  (if (op? e) (weight e) 1))


(def base-context (->Context 1 1 1 0))


(defn cycle-events
  "Like clojure.core/cycle but, moves events cyclically forward in time as it cycles"
  ([[loop-period evts]] (cycle-events nil [loop-period evts]))
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


(defn event-sort
  [a b]
  (let [c (compare (:start a) (:start b))]
    (if (not= c 0)
      c
      (compare (:end a) (:end b)))))


(defn lcp
  [a b]
  (lcm (period a) (period b)))


(defn sync-loops
  "Combines loops of different period into a single loop of `lcm` period"
  [a b]
  (let [pa (period a)
        pb (period b)
        pc (lcm a b)]
    [(slice a 0 pc false {})
     (slice b 0 pc false {})]))


(defn merge-ops
  "Combines loops of different period into a single loop of `lcm` period without merging events."
  [[pa a] [pb b]]
  (let [period (lcm pa pb)
        a (cycle-events (/ period pa) [pa a])
        b (cycle-events (/ period pb) [pb b])]
    [period (->> a
                 (concat b)
                 (sort event-sort))]))


(defn process-pattern
  [pat-root]
  (->> (apply-op pat-root base-context)
       flatten
       (group-by :period)
       (reduce merge-ops)
       (apply ->EventLoop)))




(defn display-pat
  [pat]
  (let [p (process-pattern pat)]
    p))


(defn weigh-children
  "Adds the weights of all children of an op recursively."
  [children]
  (reduce + (map weigh children)))


(defn apply-timing
  "Given a collection of events and a timing `Context`, recursively schedules
  the events honoring weights."
  [events ^Context {:keys [period spacing segment-length start] :as context}]
  (loop [raw events start start timed []]
    (if-not
        (seq raw) timed
        (let [[evt & raw] raw
              evt-weight  (weigh evt) ;; How many segments to occupy
              evt-length  (* evt-weight segment-length)
              op-ctx      (assoc context
                                 :start start
                                 :segment-length evt-length) ;; Context for children operations
              next-start  (+ start (* evt-weight spacing))]
          (recur raw next-start

                 (conj timed
                       (if (op? evt)
                         (apply-op evt op-ctx) ;; If child is an op, apply with inherited context
                         ;; Otherwise, add timing information to the event. Event keys can override (e.g., `length`).
                         (let [evt (if (map? evt) evt {:value evt})]
                           (merge {:start  start
                                   :period period
                                   :length evt-length
                                   :end    (+ start evt-length)} evt)))))))))



(defprotocol Loop
  (period [this])
  (slice [this from to offset? opts]))


(defprotocol Evented
  (events [this]))


(defrecord SinLoop [min max p]
  Loop
  (period [_] p)
  (mult [this x] x)
  (slice [_ from to offset? {}]
    (let [TAU (* 2 Math/PI)
          len (- to from)
          from (if offset? 0 from)
          to (if offset? to (- to from))
          mult (-> max (- min) (/ 2))
          base (-> from (* TAU) (/ p) Math/sin (+ 1) (* mult))
          n (+ base min)]
      [{:start from :length len :end to :n n}])))


(defrecord RandLoop [min max]
  Loop
  (period [_] 1)
  (slice [_ from to offset? {}]
    (let [len (- to from)
          from (if offset? 0 from)
          to (if offset? to (- to from))
          cap (- max min)
          n #(- (rand cap) min)]
      [{:start from :length len :end to :n n}])))


(let [rl (->RandLoop 0 10)]
  ((->
    (slice rl 0 1/4 false {})
    (nth 0)
    :n)))


(comment
  (let [sin (->SinLoop 0 100 2)]
    (for [i (range 0 9/4 1/8)]
      (slice sin i 2 false {}))))


(defrecord EventLoop [p events]
  Evented
  (events [_] events)
  Sliceable
  (period [_] p)
  (mult [this x] (-> this
                     (update :period #(* x))
                     (update :events #(cycle-events this)))) ; todo
  (slice [_ from to offset? {:keys [mode] :or {mode :begin}}]
    (let [to                (if (<= to from) (+ to p) to)
          loop              (if (> to p) (cycle-loop [p events]) events)
          [key1 key2 compr] (case mode
                              :begin  [:start :start >]
                              :end    [:end :end >]
                              :active [:end :start >=])
          slc               (into []
                                  (comp
                                   (drop-while #(compr from (key1 %)))
                                   (take-while #(> to (key2 %))))
                                  loop)]
      (if offset?
        (->> slc
             (mapv (fn [e] (update e :start #(- % from))))
             (mapv (fn [e] (update e :end #(- % from)))))

        slc))))


;; Pattern API: Functions used in pattern definition

#_(defn slice
  "Given values `from` and `to` representing cycle-relative starts (i.e, produced by `s->pos`
  returns a slice of a loop falling within the two points in time.

  If `to` is > `loop-period`, the loop is (lazily) cycled to the length needed.

  When `offset?` is true, cycle-relative times are offset by `from` to produce start-relative times for scheduling."
  [[period loop] from to & {:keys [offset? mode]
                            :or   {offset? false
                                   mode    :begin}}]
  (let [to                (if (<= to from) (+ to period) to)
        loop              (if (> to period) (cycle-loop [period loop]) loop)
        [key1 key2 compr] (case mode
                            :begin  [:start :start >]
                            :end    [:end :end >]
                            :active [:end :start >=])
        slc               (into []
                                (comp
                                 (drop-while #(compr from (key1 %)))
                                 (take-while #(> to (key2 %))))
                                loop)]
    (if offset?
      (->> slc
           (mapv (fn [e] (update e :start #(- % from))))
           (mapv (fn [e] (update e :end #(- % from)))))

      slc)))

(defn merge-with-fn
  [a b]
  (cond
    (and (number? a) (number? b)) (+ a b)
    :else [a b]))


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
  [with-fn a ^EventLoop b ^Loop & {:keys [structure-from]
                                   :or   {structure-from :left}}]
  (let [[a [period b]] (sync-loops a b)
        slice-mode     (case structure-from :left :begin :both :active)
        merged
        (reduce
         (fn [result e]
           (let [from    (:start e)
                 to      (+ from (:length e))
                 overlap (slice b from to false {:mode slice-mode})]
             (concat result (mapv #(with-fn % e) overlap))))
         []
         (.events a))]
    [period merged]))


(defn merge-loops
  [with-fn a ^EventLoop b ^Loop & {:keys [structure-from]
                                   :or   {structure-from :left}}]
  (let [new-period (lcm (period a) (period b))
        slice-mode (case structure-from :left :begin :both :active)
        merged
        (reduce
         (fn [result e]
           (let [from    (:start e)
                 to      (+ from (:length e))
                 overlap (slice b from to false {:mode :active})]
             (concat result (mapv #(with-fn e %) overlap))))
         []
         (.events a))]
    (->EventLoop new-period merged)))


(defmethod print-method EventLoop [v ^java.io.Writer writer]
  (.write writer (with-out-str (print-table (.events v)))))




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


;; Effects

