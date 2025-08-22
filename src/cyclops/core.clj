(ns cyclops.core
  "((((Tidal wave of parens))))"
  (:require
   [overtone.osc :as osc]
   [overtone.at-at :as at]
   [cyclops.events :as e]
   [cyclops.merge :as m]))

;; Constants

(def default-port 57120)
(def default-host "localhost")
(def default-cps 1)
(def default-latency-s 0.1)
(def freq-s 1/10)

;; Mutables

(def cps (atom default-cps))
(def cur-cycle (atom 0))
(def start-time (atom nil))
(def verbose (atom false))
(def debug-osc (atom false))
(def sh (atom false))

(def pool (at/mk-pool))
(def job (atom nil))

(def layers (atom {}))

;; Dirt

(def default-event
  {:_id_ (int 1)
   :s nil
   :cps (float default-cps)
   :cycle 0.0
   :delta 0.5
   :room 0.0
   :size 0.0
   :legato 1.0
   :orbit (int 0)
   :pan 0.5
   :n (int 0)
   :latency default-latency-s})

(declare pos->s)

(defn create-event
  [{:keys [start length legato] :or {start 0.0
                                     length 1/2
                                     legato 1} :as event-map}]
  (let [event (-> event-map
                  (update :delta #(or % (pos->s (* length legato))))
                  (update :s #(if (fn? %) (name (%)) (name %))))]
    (-> default-event
        (merge event)
        (update :latency #(+ % start))
        (select-keys (keys default-event)))))


(def osc-client (atom nil))

(defn send-event
  [event-map]
  (when @debug-osc (println event-map))
  (try
    (apply osc/osc-send @osc-client
           "/dirt/play"
           (mapcat (fn [[k v]] [(name k) (if (number? v) (float v) v)]) event-map))
    (catch Exception e
      (println (str "OSC ERROR: " (.getMessage e))))))


(defn send-dirt
  [event-map]
  (when (:s event-map)
    (send-event (create-event event-map))))

(comment (send-event {:s nil :start 0 :period 1}))

(defn throw-dirt
  ([evts] (throw-dirt 0.0 evts))
  ([orbit evts]
   (dorun (for [e evts]
            (future
              (if (coll? (:s e))
                (dorun (for [s (:s e)]
                          (send-dirt (assoc e :s s :orbit (float orbit)))))
                (send-dirt (assoc e :orbit (float orbit)))))))))


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
  "Modulated loop position based on number of seconds passed. Always will be < loop-period."
  [s period]
  (let [n-cycles (s->cycles s) ;; how many cycles passed
        modo (mod n-cycles period)] ;; modulate to wrap around the loop
    modo))



;; NOTE: Without syncing loops, you could get active events by adding (mod cycle period)
;; NOTE: Unary functions could be used in non-terminal merge positions and composed
;; `[inc] [(p + 10)] [(irand 10)] [5]` for merge left or flipped for merge right
;; Merge could add/substract all numerics, apply/compose fns, call argless procs
;; (layer 0 (grp merge-fn (ctrl (pat & vals)) (ctrl (pat & vales))) (grp merge-fn (ctrl pat & vals) ... ) ...))
;; or
;; (layer 0 (ctrl & pats))
;; (layer 0 (ctrl & pats) (ctrl & pats)) ;; default merge fn
;;
;; On-tick: Slice on grp calls slice on ctrl (calls slice on gen, etc), merges
;; results with merge fn (or default, which adds numerics, applies/composes fns,
;; stacks non-nums) or stack, which stacks everything) and sends results to output


(defn tick!
  "Shhh's heartbeat. Takes a single value `now` representing the 'ideal' execution time (to avoid drift), then:

  - Schedules itself for `now` plus `freq-s` (`next-tick`)
  - Calculates time since start of looping
  - Calculates events that should be scheduled between `now` and `next-tick` for each loop period (`slice`)
  - Sends the OSC messages for slices.

  NOTE: Start is currently calculated per period, which seems redundant, but
  cycling the loops and maintaining and single period is probably worse for
  performance.
"
  [now]
  (let [next-tick (+ now (* freq-s 1000))
        delta-s (/ (- now @start-time) 1000)]

    (reset! job (at/at next-tick #(tick! next-tick) pool))

    (doseq [[orbit cycl] @layers]
      #_future
      (let [period    (e/period cycl)
            from      (s->pos delta-s period)
            slice-len (s->cycles freq-s)
            to        (+ from slice-len)
            tc        (e/tc from to)
            slc       (e/slice cycl tc)
            slc       (e/offset-slice from slc)]
        (when (seq slc)
          (when @verbose
            (println from to slice-len (mapv #(select-keys % [:start :s :n]) slc)))
          (when-not @sh
            (throw-dirt orbit slc)))))))

;; Controls

(defn init-client!
  "Creates an osc client if one doesn't exist."
  ([] (init-client! default-port))
  ([port] (init-client! default-host port))
  ([host port] (when-not @osc-client (reset! osc-client (osc/osc-client host port)))))



(defn pause!
  "Stops `tick!`, but does not reset the clock."
  []
  (at/kill @job)
  (println "---PAUSED---"))


(defn continue!
  "unpauses"
  []
  (when (nil? (at/scheduled-jobs pool))
    (tick! (at/now))))


(defn sh!
  "Stops sending events, but continues processing them."
  []
  (reset! sh true))


(defn speak!
  "Un-shes"
  []
  (reset! sh false))


(defn clear-pattern!
  []
  (reset! layers {}))


(defn start!
  "Starts/restarts Shhh.

  - Inits OSC client if it hasn't been.
  - Sets `stop` to false
  - Starts tick at `now`

  NOTE: Resets the clock.
"

  []
  (pause!)
  (init-client!)
  (speak!)
  (reset! start-time (at/now))
  (tick! @start-time))


(defn restart!
  []
  (clear-pattern!)
  (start!))


(defn play! []
  (speak!)
  (continue!))



(defn o [n cyc]
  (swap! layers assoc n cyc))
