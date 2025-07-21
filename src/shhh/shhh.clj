(ns shhh.shhh
  "((((Tidal wave of parens))))"
  (:require [overtone.osc :as osc]
            [overtone.at-at :as at]))

-
(def default-port 57120)
(def default-host "localhost")
(def default-cps 0.5625)
(def default-latency-s 0.1) ;; 1/10s
(def freq-s 0.1) ;; 1/10s

(def cps (atom default-cps))
(def cur-cycle (atom 0))
(def start-time (atom nil))
(def stop (atom false))

(def pool (at/mk-pool))

(def evts (atom []))

(def default-event
  {:_id_ (int 1)
   :s nil
   :cps 0.5625
   :cycle 0.0
   :delta 0.0
   :orbit (int 0)
   :n (int 0)
   :latency 0.0})


(defn create-event
  [event-map]
  (-> default-event
      (merge event-map)
      (update :latency #(+ % default-latency-s))
      (select-keys (keys default-event))))


(comment (create-event {:s "bd"}))


(def osc-client (atom nil))

(defn init-client!
  ([] (init-client! default-port))
  ([port] (init-client! default-host port))
  ([host port] (when-not @osc-client (reset! osc-client (osc/osc-client host port)))))

(defn send-event
  [event-map]
  (apply osc/osc-send @osc-client
         "/dirt/play"
         (mapcat (fn [[k v]] [(name k) v]) event-map)))


(defn send-dirt
  [event-map]
  (println "Sending: " event-map)
  (send-event (create-event event-map)))


(defn throw-dirt [evts]
  (doseq [e evts] (send-dirt e)))


(defn s->cycles
  [s]
  (* @cps s))


(defn s->pos [s]
  (let [abs (s->cycles s)
        cycle (Math/floor abs)
        rel (- abs cycle)]
    [abs cycle rel]))


(comment (s->pos 3))

(comment
  (reset! cps 0.5)
  (s->pos 4.75))


(defn dbl
  [evts]
  (->> evts
       (map #(update % :whole inc))
       (concat evts)
       vec))


(defn slice [evts from to]
  (->> evts
       (dbl)
       (drop-while #(>= from (:whole %)))
       (take-while #(> to (:whole %)))))



(defn pos->s [pos]
  (/ (float pos) @cps))


(defn time-evts [pos evts]
  (mapv
   #(let [rel    (:whole %)
          offset (- rel pos)
          s      (pos->s offset)
          target (float s)]
      (assoc % :latency target))
   evts))


;; `@start-time` is when we started looping
;; `now` is the current time
;; `now - @start-time = delta` how long we've been looping
;; `(s->pos delta)` is the periodic cycle position represented by `delta`
;; `freq-s` is how far forward we sample events


(defn tick!
  []
  (let [now (at/now)]
    (when-not @start-time
      (reset! start-time (inc now)))
    (let [delta-s (/ (- now @start-time) 1000.0)
          [_abs _cycle start]   (s->pos delta-s)
          [len _cycle _rel]     (s->pos freq-s)
          end     (+ start len)
          slc     (slice @evts start end)
          sched   (time-evts start slc)
          lst     (-> sched last :latency)
          nxt     (or lst freq-s)]
      (when (seq sched)
        (clojure.pprint/pprint
         {:now   now
          :delta delta-s
          :start start
          :end   end
          :len   len
          :slice slc
          :sched sched})
        (throw-dirt sched))
      (when (not @stop)
        (at/at (+ now (* nxt 1000)) #'tick! pool)))))


(comment
  (do
    (reset! start-time nil)
    (tick!)
    (reset! stop true)))


(defn start! []
  (init-client!)
  (reset! stop false)
  (reset! start-time nil)
  (#'tick!))


(defn stop! []
  (reset! stop true)
  (println "---STOPPED---"))



(defn fastcat
  "Concatenates a series of events into `cycles` cycles starting at `offset`.
Default behavior (cycles=1, offset=0) crams the events into one cycle."
  ([pat-seq] (fastcat pat-seq 1 0))
  ([pat-seq cycles offset]
   (let [n (count pat-seq)
         denom (* n cycles)]
     (map-indexed (fn [idx evt] (assoc evt :whole (+ offset (/ idx denom)))) pat-seq))))


(comment



  (let [pat [{:s "bd"} {:s "sd"} {:s "sd"}]]
    ;; (reset! evts (pat-seq->evts pat))
    (reset! evts [{:s "supermandolin" :n 0.0 :whole 0} {:s "sd" :whole 1/4} {:s "sd" :whole 3/8} {:s "bd" :whole 1/2} {:s "sd" :whole 3/4}])
    (reset! evts [{:s "supermandolin" :n 0.0 :whole 0 :delta 0.44444400072098}
                  {:s "supermandolin" :n 9.0 :whole 1/4 :delta 0.44444400072098}
                  {:s "supermandolin" :n 5.0 :whole 1/2 :delta 0.44444400072098}
                  {:s "supermandolin" :n 4.0 :whole 3/4 :delta 0.44444400072098}])
    (reset! cps 0.5)
    (reset! stop false)
    (reset! start-time nil)
    (tick!))

  (stop!)


  (shhh)
  (init-client!)

  (send-dirt {:s "bd"})

  (throw-dirt [{:s "bd"}])

  (start!)

  (reset! evts [{:s "sd"} {:s "bd" :latency (/ @cps 2.0)}])

  (set-cps! default-cps)

  (shhh)

  @heartbeat)
