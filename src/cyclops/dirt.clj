(ns cyclops.dirt
  (:require
   [overtone.sc.server :as server]
   [overtone.osc :as osc]
   [cyclops.core :as c :refer [dispatch]]
   [overtone.at-at :refer [now]]
   ;; [cyclops.util :refer [reduplicate]]
   ;; [cyclops.sc :as sc]
   ))


(comment
  (def osc-client (osc/osc-client "localhost" 57120))
  (osc/osc-send osc-client "/dirt/play" "s" "bd" "_id_" (int 1) "latency" 0.05))

(def debug (atom false))
(def dirt-path "/dirt/play")
(def default-latency-s 0.05)


(def allowed-keys
  #{:_id_ :s :cps :cycle :delta :room :size :orbit :pan :n :latency})


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


(defn ->dirt-map
  [{:keys [trigger-at length params layer]
    :or   {trigger-at (now) length 1 layer 0}}]
  (-> default-event
      (merge params)
      (update :delta #(or % length))
      (update :latency #(+ % (/ (- trigger-at (now)) 1000.0)))
      (assoc :orbit layer)
      (select-keys allowed-keys)))


(defmethod dispatch :dirt
  [evt]
  (let [dm (->dirt-map evt)]
    (try
      (apply osc/osc-send osc-client
             "/dirt/play"
             (mapcat (fn [[k v]] [(name k) (if (number? v) (float v) v)]) dm))
      (catch Exception e
        (println (str "OSC ERROR: " (.getMessage e)))))))



(osc/osc-msg dirt-path "_id_" 1)
