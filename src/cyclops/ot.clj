(ns cyclops.ot
  (:require [cyclops.core :refer [dispatch]]
            [overtone.studio.core]
            [overtone.studio.mixer :as mixer]
            [overtone.studio.transport :refer [*clock*]]
            [overtone.libs.event :refer [event]]))

(comment
  (mixer/boot-server-and-mixer)
  (require '[overtone.inst.drum :refer [kick snare]]))


(defn load-sounds
  []
  (require '[overtone.inst.drum :refer :all])
  (require '[overtone.inst.sampled-piano :refer [sampled-piano]]))

(comment (load-sounds)
         (dub-kick))


(def dirt-map
  {"bd" dub-kick
   "drum" kick
   "sd" snare
   "ch" hat3
   "oh" open-hat
   "hh" hat-demo
   "piano" sampled-piano
   "cp" clap})


(defn dirt->ot
  [evt cycl-num]
  (-> evt
      (assoc :clock *clock*)
      (assoc :beat (+ cycl-num (evt :start)))
      (assoc :midinote (:n evt))
      (assoc :instrument (dirt-map (evt :s)))))


(comment (dirt->ot {:start 1/2 :s :bd} 10))

(defmethod dispatch :overtone
  [{:keys [evt cycl-num _orbit]}]
  (println evt)
  (when (:s evt)
    (let [cycl-num (or cycl-num (*clock*))
          ot-evt (dirt->ot evt cycl-num)]
      (event :note ot-evt))))



(comment
  (event :note {:instrument kick :clock *clock* :beat (*clock*) :orbit 0 :target :overtone :start 0})

  (let [cycl-num (*clock*)]
    (dispatch {:target :overtone :evt {:start 0 :s :bd} :cycl-num cycl-num :orbit 0})))
