(ns cyclops.core
  "((((Tidal Wave of Parens))))"
  (:require
   [overtone.at-at :as at]
   [cyclops.events :as e]
   [cyclops.util :refer [reduplicate]]
   [cyclops.ops :as ops]
   [cyclops.core :as c]
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
  (* cps 60))

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


(defmulti dispatch :target)


(def defaults (atom {:target :default}))


(defn dispatch*
  [evts ctx]
  (doseq [es evts]
    (doseq [e (reduplicate es)]
      (dispatch (merge @defaults ctx e)))))


(defmethod dispatch :default
  [{:keys [start-time] :or {start-time 0} :as evt}]
  (at/at start-time
         #(let [r (e/realize evt evt)]
            (println r))
         pool))


(comment
  (dispatch {:params {:init (rand)} :start-time (+ 1000 (at/now))})
  (dispatch* [{"s" "bd"}] {}))


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
  (/ (float pos) @cps))


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
          slc    (e/offset (- from) slc)]
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
   (map #(assoc % :trigger-at (*clock* (+ cycle-num (:start %))))
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
  (swap! layers assoc n (hoist-merge cyc)))


(defn now! [& cyc]
  (-> cyc hoist-merge ops/evts apply-timing (dispatch* {})))


(defn once [& cyc]
  (-> cyc hoist-merge ops/evts (apply-timing (*clock*)) (dispatch* {})))
