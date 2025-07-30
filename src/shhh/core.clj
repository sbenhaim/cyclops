(ns shhh.core
  "((((Tidal wave of parens))))"
  (:require
   [clojure.pprint :refer [pprint]]
   [clojure.string :as s]
   [clojure.math.numeric-tower :refer [lcm]]
   [overtone.osc :as osc]
   [overtone.music.pitch :as m]
   [overtone.at-at :as at]
   [shhh.ops :as ops]))


;; Constants

(def default-port 57120)
(def default-host "localhost")
(def default-cps 5625/10000)
(def default-latency-s 0.1)
(def freq-s 1/10)

;; Mutables

(def cps (atom default-cps))
(def cur-cycle (atom 0))
(def start-time (atom nil))
(def verbose (atom true))
(def debug-osc (atom false))
(def stop (atom false))
(def shhh (atom false))

(def pool (at/mk-pool))

(def loops (atom {1 []}))

;; Dirt

(def default-event
  {:_id_ (int 1)
   :s nil
   :cps (float default-cps)
   :cycle 0.0
   :delta 0.0
   :room 0.0
   :size 0.0
   :orbit (int 0)
   :n (int 0)
   :latency default-latency-s})


;; (defmulti coerce-event first)

;; (defmethod coerce-event :default [k v e]
;;   (dissoc e k))

;; (defmethod coerce-event :latency [k v e]
;;   (update e k
;;           #(-> e :whole (+ %) float)))

;; (defmethod coerce-event :s [_ v _]
;;   (name v))

;; (defmethod coerce-event :delta [k v e]
;;   (update e k float))

(defn create-event
  [{:keys [whole] :or {whole 0.0} :as event-map}]
  (-> default-event
      (merge event-map)
      (update :latency #(-> % (+ whole) float)) ;; Remember the joy of live coding
      (update :s name)
      (update :n float)
      (update :delta float)
      (select-keys (keys default-event))))


(def osc-client (atom nil))

(defn send-event
  [event-map]
  (when @debug-osc (println event-map))
  (try
    (apply osc/osc-send @osc-client
           "/dirt/play"
           (mapcat (fn [[k v]] [(name k) v]) event-map))
    (catch Exception e
      (println (str "OSC ERROR: " (.getMessage e))))))


(defn send-dirt
  [event-map]
  (send-event (create-event event-map)))

(comment (send-event {:s "bd" :whole 0 :order 1}))

(defn throw-dirt [evts]
  (doseq [e evts] (send-dirt e)))


;; Events, cycles loops


(defn s->cycles
  "Converts seconds to number of cyles based on current cps"
  [s]
  (* @cps s))


(defn pos->s
  "Given a cycle-relative position, returns seconds based on current cps"
  [pos]
  (/ (float pos) @cps))


(defn event?
  [e]
  (map? e))


(defn s->pos
  "Modulated loop position based on number of seconds passed. Always will be < loop-order."
  [s loop-order]
  (let [n-cycles (s->cycles s) ;; how many cycles passed
        modo (mod n-cycles loop-order)] ;; modulate to wrap around the loop
    modo))


(comment
  (reset! cps 0.5)
  (s->pos 4 [{:whole 1/3} {:whole 1}]))


(defn cycle-loop
  "Like clojure.core/cycle but, moves events cyclically forward in time as it cycles"
  ([loop-order evts] (cycle-loop nil loop-order evts))
  ([n loop-order evts]
   (let [loop (->> evts
                   repeat
                   (mapcat
                    (fn [i cycle]
                      (for [e cycle]
                        (update e :whole #(+ % (* i loop-order)))))
                    (iterate inc 0)))]
     (if n
       (take (* n (count evts)) loop)
       loop))))


(comment (->> [{:whole 0} {:whole 3/2} {:whole 4}] (cycle-loop 3) (take 20)))


(defn cycle-loops
  "Turn a hashmap of {loop-order loop} into a hashmap of infinite lazy cycling loops.

NOTE: Not currently used in favor of JIT cycling in `slice`. May improve performance."
  [loops]
  (into {}
        (for [[loop-order evts] loops]
          [loop-order (cycle-loop loop-order evts)])))


(defn slice
  "Given values `from` and `to` representing cycle-relative positions (i.e, produced by `s->pos`
  returns a slice of a loop falling within the two points in time.

  If `to` is > `loop-order`, the loop is (lazily) cycled to the length needed.

  Cycle-relative times are offset by `from` to produce position-relative times for scheduling."
  [[order loop] from to]
  (assert (> to from))
  (let [loop (if (> to order) (cycle-loop order loop) loop)]
    (into []
          (comp
           (drop-while #(> from (:whole %)))
           (take-while #(> to (:whole %)))
           (map (fn [e] (update e :whole #(- % from)))))
          loop)))




(defn tick!
  "Shhh's heartbeat. Takes a single value `now` representing the 'ideal' execution time (to avoid drift), then:

  - Schedules itself for `now` plus `freq-s` (`next-tick`)
  - Calculates time since start of looping
  - Calculates events that should be scheduled between `now` and `next-tick` for each loop order (`slice`)
  - Sends the OSC messages for slices.

  NOTE: Position is currently calculated per order, which seems redundant, but
  cycling the loops and maintaining and single order is probably worse for
  performance.
"
  [now]
  (let [next-tick (+ now (* freq-s 1000))
        delta-s (/ (- now @start-time) 1000)]

    (when-not @stop
      (at/at next-tick #(tick! next-tick) pool))

    (run!
     (fn [[order loop]]
       (do
         (let [pos-from  (s->pos delta-s order)
               slice-dur (s->pos freq-s order)
               pos-to    (+ pos-from slice-dur)
               slc       (slice [order loop] pos-from pos-to)]
           ;; Todo: Convert note position to scheduling latency
           (when (seq slc)
             (when @verbose
               (pprint
                {:now       now
                 :delta-s   delta-s
                 :freq-s    freq-s
                 :from      pos-from
                 :to        pos-to
                 :slice-len slice-dur
                 :next-tick next-tick
                 :slice     slc
                 }))
             (when-not @shhh
               (throw-dirt slc))))))
     @loops)))


;; Loop Maths


(defn shift
  "Moves a collection of events by a cycle-relative offset."
  [evts offset]
  (for [e evts]
    (if (event? e)
      (update e :whole #(+ offset %))
      (shift e offset))))


(comment (shift [{:whole 0} [{:whole 1/2} {:whole 3/4}]] 1/4))


(defn interleave-loops
  "Combines loops of different order into a single loop of `lcm` order"
  [[order-a loop-a] [order-b loop-b]]
  (let [lcm (lcm order-a order-b)
        ab  (concat (cycle-loop (/ lcm order-a) loop-a)
                    (cycle-loop (/ lcm order-b) loop-b))
        pat (sort-by :whole ab)]
    pat))


;; Patterns



(comment

  (defrecord TimingContext
      [offset
       offset-multiplier
       loop-order
       segment-order])

  (comment (ns-unmap *ns* 'apply-op))

  ;; TODO: Replace with ops that only return tc and don't apply times

  (defmulti apply-op
    "Given an op sub-pattern in the form of `[op & args & events]` and the the TimingContext from the
  parent sub-pattern, returns the collection of events (without `op` and `args`) and a new
  TimingContext for scheduling them."
    (fn [[op & _args+events] _parent-context] op))

  (defmethod apply-op :default [_subpat tc]
    tc)





  (defn assign-times
    ([sub-pattern] (assign-times sub-pattern ops/initial-context))
    ([sub-pattern ^ops/TimingContext parent-context]
     (let [[events ^ops/TimingContext tc] (ops/apply-op sub-pattern parent-context)]
       (map-indexed
        (fn [i e]
          (let [pos (+ (:offset tc) (* i (:offset-multiplier tc)))]
            (if (event? e)
              (assoc e :whole pos :order (:loop-order tc))
              (assign-times e (assoc tc :offset pos)))))
        events)))))


(comment
  (defmethod apply-op :fast
    [[_op & events] {:keys [offset loop-order segment-order]}]
    (let [n                     (count events)
          new-segment-order     (/ segment-order n)
          new-offset-multiplier new-segment-order]
      [events (->TimingContext offset
                               new-offset-multiplier
                               loop-order
                               new-segment-order)]))


  (defmethod apply-op :slow
    [[_op & events] {:keys [loop-order segment-order offset]}]
    (let [new-offset-multiplier loop-order
          new-loop-order        (* loop-order (count events))]
      [events (->TimingContext offset
                               new-offset-multiplier
                               new-loop-order
                               segment-order)])))


(defn untangle-loops
  "After assigning event times to a pattern, converts nested loops of various order
  into a hashmap of {loop-order loop}."
  [tangled]
  (->> tangled
       flatten
       (sort-by :whole)
       (group-by :order)))


(defn process-pattern
  "Takes a pattern and returns a hashmap of {loop-order loop}."
  [pat]
  (-> pat
      (assign-times)
      untangle-loops))

;; Controls

(defn init-client!
  "Creates an osc client if one doesn't exist."
  ([] (init-client! default-port))
  ([port] (init-client! default-host port))
  ([host port] (when-not @osc-client (reset! osc-client (osc/osc-client host port)))))



(defn pause!
  "Stops `tick!`, but does not reset the clock."
  []
  (reset! stop true)
  (println "---STOPPED---"))


(defn continue!
  "unpauses"
  []
  (reset! stop false)
  (tick! (at/now)))


(defn shhh!
  "Stops sending events, but continues processing them."
  []
  (reset! shhh true))


(defn speak!
  "Un-shhhes"
  []
  (reset! shhh false))

(defn set-pattern!
  [pat]
  (reset! loops (process-pattern pat)))


(defn clear-pattern!
  []
  (reset! loops {}))


(defn start!
  "Starts/restarts Shhh.

  - Inits OSC client if it hasn't been.
  - Sets `stop` to false
  - Starts tick at `now`

  NOTE: Resets the clock.
"

  []
  (init-client!)
  (pause!)
  (Thread/sleep (* freq-s 1000 2))
  (continue!)
  (speak!)
  (reset! start-time (at/now))
  (tick! @start-time))


(defn restartt!
  []
  (clear-pattern!)
  (start!))


(defn play! [pat]
  (set-pattern! pat)
  (init-client!)
  (speak!)
  (continue!))

(re-find #"(.*)(\d)" "c#4")

(defn note
  ([n]
   (let [n      (name n)
         o (re-find #"(.*)(\d)" n)]
     (if o (note (second o) (-> o last parse-double))
        (note n 4))))
  ([n o]
   (m/note (str (name n) o))))



(re-find #"(.*)(\d)" "c")

(comment (note :c#))


(comment
  (start!)
  (clear-pattern!)
  (reset! debug-osc false)
  (reset! verbose false)
  (play!
   (into [:fast] (for [n [:c :a :f :e]]
                   {:s "superpiano" :n (note n 5) :delta 2.0 :size 0.7 :room 0.7})))
  (shhh!)
  (speak!)
  (clear-pattern!)


  (send-dirt {:s "midi" :n (note :d#5) :delta 2.0})


  )





;; Pattern parsing


(defn mini-parse
  [e]
  (cond
    (= (type e) clojure.lang.Cons) (into [:slow] (mapv mini-parse e))
    (coll? e) (into [:fast] (mapv mini-parse e))
    (number? e) (-> e str keyword)
    (symbol? e) (-> e name keyword)
    :else (-> e name keyword)))


(defn mini
  [syms]
  (let [pat (mapv mini-parse syms)]
    (if (keyword? (first pat))
      pat
      (into [:fast pat]))))


(defn str*
  [& args]
  (s/join " " args))


(defn mini-str-parse
  [e]
  (cond
    (= (type e) clojure.lang.Cons) (str "< " (apply str (mapv mini-str-parse e)) ">")
    (coll? e) (str "[" (apply str* (mapv mini-str-parse e)) "]")
    (number? e) (str e)
    :else (-> e name str)))


(defn mini-str
  [syms]
  (apply str* (mapv mini-str-parse syms)))


