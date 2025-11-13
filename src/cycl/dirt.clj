(ns cycl.dirt
  (:require
   [overtone.osc :as osc]
   [cycl.ing :as c :refer [dispatch]]
   [cycl.util :as u]
   [overtone.at-at :refer [now]]
   [mount.core :as mount :refer [defstate]]
   [clojure.pprint :refer [pprint]]
   [clojure.string :as str]))


(defstate client
  :start (osc/osc-client "localhost" 57120))


(comment (mount/start))

(comment
  (def osc-client (osc/osc-client "localhost" 57120))
  (osc/osc-send osc-client "/dirt/play" "s" "bd" "_id_" (int 1) "latency" 0.05))

(def debug (atom false))
(def dirt-path "/dirt/play")
(def default-latency-s 0.05)


(def allowed-keys
  #{:_id_ :s :cps :cycle :delta :room :size :orbit :pan :n :latency :note :speed
    :voice :decay :accelerate :semitone :resonance :lfo :rate :pitch1 :pitch2 :pitch3
    :slide :detune :muffle :stereo :perc :percf :modamp :modfreq :octave
    :lfofreq :lfodepth :amp :ratio :eglevel :egrate ;; ???
    :midinote :freq :sustain :in :inr
    :chop
    })


(def default-event
  {:_id_ (int 1)
   :s nil
   :cps @c/cps
   :cycle 0.0
   :delta 0.5
   :room 0.0
   :size 0.0
   :orbit (int 0)
   :pan 0.5
   :n (int 0)
   :latency default-latency-s})


(defn split-sound-sample
  [s]
  (let [[s n] (str/split s #":")]
    (if n {:s s :n (parse-long n)}
        {:s s})))


(defn ->dirt-map
  [{:keys [trigger-at length params layer]
    :or   {trigger-at (now) length 1 layer 0}}]
  (when-let [s (:s params)]
    (-> default-event
        (merge params)
        (merge (split-sound-sample s))
        (update :s name)
        (update :delta #(or % length))
        (update :latency #(+ % (/ (- trigger-at (now)) 1000.0)))
        (assoc :orbit layer)
        (select-keys allowed-keys))))


(defn conform [v]
  (cond
    (int? v)     (int v)
    (keyword? v) (name v)
    :else        v))


(comment
  (type
   (conform "string")))

(defmethod dispatch :dirt
  [_target evts _ctx]
  (let [dirt-msgs (keep ->dirt-map evts)]
    (when (seq dirt-msgs)
      (when @debug
        (println "Bundling" (count dirt-msgs) "events for SuperDirt"))
      (try
        (let [bundle-time (now)
              msgs (for [dm dirt-msgs]
                     (apply osc/osc-msg
                            "/dirt/play"
                            (mapcat (fn [[k v]] [(name k) (conform v)]) dm)))
              bundle (apply osc/osc-bundle bundle-time msgs)]
          (osc/osc-send-bundle client bundle))
        (catch Exception e
          (println (str "OSC BUNDLE ERROR: " (.getMessage e))))))))

(defn connect-dirt
  []
  (mount/start))

(comment
  (u/toggle! debug)
  (mount/start)
  (dispatch :dirt [{:target :dirt :params {:s "piano" :n 5 :note 0 :decay 1.0} :trigger-at (now)}] {}))


