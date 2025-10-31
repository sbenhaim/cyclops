(ns cycl.ing
  (:require
   [overtone.at-at :as at]
   [cycl.events :as e]
   [cycl.util :refer [reduplicate]]
   [cycl.ops :as ops]
   [cycl.core :as c]
   [overtone.studio.transport :refer [*clock*]]
   [overtone.music.rhythm :as metro]
   [overtone.music.time :refer [apply-by]]))


;; Constants

(def default-port 57120)
(def default-host "localhost")
(def default-cps 1)
(def default-latency-s 0.1)
(def tick-dur 1/10)

(defn cps->bpm [cps]
  (* cps 60.0))

(defn bpm->cps [bpm]
  (/ bpm 60.0))

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


(defmulti dispatch
  "Dispatches a collection of timed events (a slice) to a target.
  Each event includes :trigger-at (absolute timestamp in ms)."
  (fn [target _evts _ctx] target))


(def defaults (atom {:target :dirt}))


(defn dispatch*
  "Dispatches a collection of events, grouping by target for atomic delivery."
  [evts ctx]
  (let [evts-flat (mapcat reduplicate evts)
        evts-merged (map #(merge @defaults ctx %) evts-flat)]
    (when (seq evts-merged)
      ;; Group by target and dispatch each group as a slice
      (doseq [[target target-evts] (group-by :target evts-merged)]
        (dispatch target target-evts ctx)))))


(defmethod dispatch :default
  [_target evts _ctx]
  (doseq [{:keys [trigger-at] :or {trigger-at 0} :as evt} evts]
    (at/at trigger-at
           (println evt)
           pool)))


(comment
  (dispatch :default [{:params {:init (rand)} :trigger-at (+ 1000 (at/now))}] {})
  (dispatch* [{"s" "bd" :target :dirt}] {}))


;; Dirt


(defn s->cycles
  "Converts seconds to number of cyles based on current cps"
  [s]
  (* s @cps))



(comment
  (reset! cps 1/2)
  (s->cycles 2.517))


(defn pos->s
  "Given a cycle-relative position, returns seconds based on current cps"
  [pos]
  (/ (float pos) (bpm->cps (metro/metro-bpm *clock*))))


(defn s->pos
  "Modulated loop position based on number of seconds passed. Always will be < loop-period."
  [s period]
  (let [n-cycles (s->cycles s) ;; how many cycles passed
        modo (mod n-cycles period)] ;; modulate to wrap around the loop
    modo))


(defn get-slice [cycl cycle-num]
  (when (seq cycl)
    (let [period (e/period cycl)
          from   (mod cycle-num period)
          slc    (e/slice cycl from tick-dur :starts-during)
          slc    (map #(assoc % :trigger-after (- (:start %) from)) slc)]
      (when (seq slc)
        (when @verbose
          (println from 1 (mapv #(select-keys % [:start :s :n]) slc)))
        slc))))

(defn apply-timing
  ([slc]
   (let [now (at/now)]
     (map #(assoc % :trigger-at (-> % :start pos->s (* 1000) (+ now)))
          slc)))
  ([slc cycle-num]
   (map #(assoc % :trigger-at (*clock* (+ cycle-num (or (:trigger-after %)
                                                        (:start %)))))
        slc)))


(defn tick
  ([] (tick (*clock*)))
  ([cycle-num]
   (doseq [[layer cycl] @layers]
     #_future
     (let [slc (get-slice cycl cycle-num)
           slc (apply-timing slc cycle-num)
           ctx {:cycle-num cycle-num :layer layer}
           slc (e/realize slc ctx)]
       (when (and (not @sh) (seq slc))
         (dispatch* slc ctx))))
   (let [next-cycle (+ cycle-num tick-dur)
         next-tick  (*clock* next-cycle)]
     (reset! job
             (apply-by next-tick #(tick next-cycle))))))


(comment
  (reset! layers {0 [{:start 0 :length 1 :period 2 :params {:init :a}}]})
  (reset! layers {0 []})
  (metro/metro-bpm *clock* 60)
  (tick)
  (at/kill @job))


;; Controls

#_(defn init-client!
  "Creates an osc client if one doesn't exist."
  ([] (init-client! default-port))
  ([port] (init-client! default-host port))
  ([host port] (when-not @osc-client (reset! osc-client (osc/osc-client host port)))))



(defn pause!
  "Stops `tick!`, but does not reset the clock."
  []
  (at/kill @job)
  (println "---PAUSED---"))


(comment (pause!))


(defn continue!
  "unpauses"
  []
  (when (nil? (at/scheduled-jobs pool))
    (tick)))

(comment (continue!))

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


(defn set-cps!
  [cps]
  (metro/metro-bpm *clock* (cps->bpm cps)))


(defn set-bpm!
  [bpm]
  (metro/metro-bpm *clock* bpm))


(defn start!
  "Starts/restarts.

  - Inits OSC client if it hasn't been.
  - Sets `stop` to false
  - Starts tick at `now`

  NOTE: Resets the clock.
"

  []
  (at/kill @job)
  (speak!)
  (metro/metro-bpm *clock* (cps->bpm @cps))
  (metro/metro-start *clock* 0)
  (tick 0))

(comment
  (metro/metro-start *clock* 0)
  (start!))


(defn restart!
  []
  (clear-pattern!)
  (start!))


(defn shutdown!
  []
  (clear-pattern!)
  (pause!))


(defn play! []
  (speak!)
  (continue!))


(defn hoist-merge
  [cyc]
  (if (> (count cyc) 1)
    (apply ops/+| cyc)
    (first cyc)))


(defn o
  [n & cyc]
  (swap! layers assoc n (hoist-merge cyc))
  (speak!))


(defn now! [& cyc]
  (-> cyc hoist-merge ops/evts apply-timing (dispatch* {})))


(defn once [& cyc]
  (-> cyc hoist-merge ops/evts (apply-timing (*clock*)) (dispatch* {})))
