(ns ot
  {:clj-kondo/ignore true})

(use '[overtone.live])

(require '[overtone.inst.drum :refer [kick kick2 kick3 kick4 dub-kick]])
(require '[overtone.inst.piano :refer [piano]])
(require '[overtone.inst.sampled-piano :refer [sampled-piano]])

(require '[overtone.music.rhythm :as r])


(doseq [f [kick kick2 kick3 kick4 dub-kick]]
  (f)
  (Thread/sleep 250))

(piano 440)

(sampled-piano 60 )



(def nome (r/metronome 120))

(nome)


(def kick (sample (freesound-path 2086)))


(ns-unmap *ns* 'kick)
(kick)

(pan2 0 1)

(let [p (mda-piano 440)]
  (p))

(inst-pan! kick 1)

(scope :buf kick)

(event :some :a :b :c :d)

(event :note :instrument sampled-piano)
(osc-debug true)
(kick)

(do
  (let [pan 0]
    (reset! (:pan kick) pan)))

(ctl (:mixer kick) :pan 1)
(kick)

(event :note :instrument sampled-piano :midinote 60)


(event-debug-on)

(require '[overtone.sc.machinery.server.comms :as comms])
(comms/server-snd " /dirt/play" :_id_    (int 1)
                  :s       "supermandolin"
                  :cps     1.0
                  :cycle   0.0
                  :delta   0.5
                  :room    0.0
                  :size    0.0
                  :orbit   (int 0)
                  :pan     0.5
                  :n       (int 0)
                  :latency 0.5)

(event :osc-msg {:path "/dirt/play"
                 :args {:_id_    (int 1)
                        :s       "supermandolin"
                        :cps     1.0
                        :cycle   0.0
                        :delta   0.5
                        :room    0.0
                        :size    0.0
                        :orbit   (int 0)
                        :pan     0.5
                        :n       (int 0)
                        :latency 0.5}})



:clock, :beat, :dur, :start-time, :end-time

(let [clock (metronome 120)]
  (dotimes [i 3]
    (event :note {:instrument sampled-piano
                  :clock clock
                  :beat (+ 0 (clock))
                  :dur 2})))
