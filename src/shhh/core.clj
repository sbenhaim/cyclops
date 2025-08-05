(ns shhh.core
  "((((Tidal wave of parens))))"
  (:require
   [clojure.math.numeric-tower :refer [lcm]]
   [overtone.osc :as osc]
   [overtone.at-at :as at]
   [shhh.pattern :as pat]))


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
   :delta 0.5
   :room 0.0
   :size 0.0
   :orbit (int 0)
   :n (int 0)
   :latency default-latency-s})

(declare pos->s)


(defn create-event
  [{:keys [position length] :or {position 0.0 length 1/2} :as event-map}]
  (-> default-event
      (merge event-map)
      (update :latency #(-> % (+ position) float)) ;; Remember the joy of live coding
      (update :s name)
      (update :n float)
      (update :delta #(or % (pos->s length)))
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

(comment (send-event {:s "bd" :position 0 :period 1}))

(defn throw-dirt [evts]
  (doseq [e evts] (send-dirt e)))


;; Events, cycles loops


(defn s->cycles
  "Converts seconds to number of cyles based on current cps"
  [s]
  (* s @cps))


(comment
  (reset! cps 1/2)
  (s->cycles 2))


(defn pos->s
  "Given a cycle-relative position, returns seconds based on current cps"
  [pos]
  (/ (float pos) @cps))


(defn s->pos
  "Modulated loop position based on number of seconds passed. Always will be < loop-order."
  [s loop-order]
  (let [n-cycles (s->cycles s) ;; how many cycles passed
        modo (mod n-cycles loop-order)] ;; modulate to wrap around the loop
    modo))



(defn cycle-loop
  "Like clojure.core/cycle but, moves events cyclically forward in time as it cycles"
  ([loop-order evts] (cycle-loop nil loop-order evts))
  ([n loop-order evts]
   (let [loop (->> evts
                   repeat
                   (mapcat
                    (fn [i cycle]
                      (for [e cycle]
                        (update e :position #(+ % (* i loop-order)))))
                    (iterate inc 0)))]
     (if n
       (take (* n (count evts)) loop)
       loop))))


(comment (->> [{:position 0} {:position 3/2} {:position 4}] (cycle-loop 3) (take 20)))


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
  ;; (assert (> to from))
  (let [to (if (<= to from) (+ to order) to)
        loop (if (> to order) (cycle-loop order loop) loop)]
    (into []
          (comp
           (drop-while #(> from (:position %)))
           (take-while #(> to (:position %)))
           (map (fn [e] (update e :position #(- % from))))
           )
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
               slice-len (s->pos freq-s order)
               ;; pos-to    (s->pos (/ next-tick 1000) order)
               pos-to    (+ pos-from slice-len)
               slc       (slice [order loop] pos-from pos-to)]
           ;; Todo: Convert note position to scheduling latency
           (when (seq slc)
             (when @verbose
               (println
                {:now       now
                 :delta-s   [delta-s (float delta-s)]
                 :freq-s    [freq-s (float freq-s)]
                 :from      [pos-from (float pos-from)]
                 :to        [pos-to (float pos-to)]
                 :slice-len [slice-len (float slice-len)]
                 :next-tick [next-tick (float next-tick)]
                 :slice     slc
                 }))
             (when-not @shhh
               (throw-dirt slc))))))
     @loops)))


;; Loop Maths


(defn interleave-loops
  "Combines loops of different order into a single loop of `lcm` order"
  [[order-a loop-a] [order-b loop-b]]
  (let [lcm (lcm order-a order-b)
        ab  (concat (cycle-loop (/ lcm order-a) loop-a)
                    (cycle-loop (/ lcm order-b) loop-b))
        pat (sort-by :position ab)]
    pat))



(defn untangle-loops
  "After assigning event times to a pattern, converts nested loops of various order
  into a hashmap of {loop-order loop}."
  [tangled]
  (->> tangled
       flatten
       (sort-by :position)
       (group-by :period)))


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
  (println "---PAUSED---"))


(defn continue!
  "unpauses"
  []
  (when @stop
    (reset! stop false)
    (tick! (at/now))))


(defn sh!
  "Stops sending events, but continues processing them."
  []
  (reset! shhh true))


(defn speak!
  "Un-shhhes"
  []
  (reset! shhh false))

(defn set-pattern!
  [pat]
  (reset! loops (-> pat
                    pat/process-pattern
                    untangle-loops)))


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
  (speak!)
  (reset! stop false)
  (reset! start-time (at/now))
  (tick! @start-time))


(defn restart!
  []
  (clear-pattern!)
  (start!))


(defn play! [pat]
  (set-pattern! pat)
  (speak!)
  (continue!))






(comment
  (start!)

  (reset! verbose false)
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
