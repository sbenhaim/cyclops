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

(piano 60)

(event :note :instrument sampled-piano :midinote 60 :release 4)

(sampled-piano 60)



(def nome 
(r/metronome 120))

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
  (doseq [b (range 4)]
    (at (clock b) (kick))
    (at (clock (+ b 1/2)) (kick2))))

overtone.studio.transport/*clock*

(overtone.studio.mixer/)

(metro-start )

(kick)

(tap> 1)
(:instruments @studio*)
:dbg
*clock*

(event :note :instrument overtone.inst.drum/snare2)

(require '[overtone.inst.drum :as d])

(let [clock (metronome 120)]
  (doseq [b (range 10)]
    (event :note :instrument d/snare :clock nil :beat b)))


(require '[overtone.studio.transport :refer [*clock*]])

(at (t/*clock* (+ (t/*clock*) 20)) #(println "h1llo"))

(at (+ (t/*clock*) 10))

[(t/*clock* (t/*clock*)) (t/*clock* (+ (t/*clock*) 10))]


(let [b (t/*clock*)]
  (apply-by (t/*clock* (+ b 3)) #(println "hello")))




(defn metro-tick
  []
  (let [n ()]))


(metro-start *clock* 0)
(metro-bpm *clock* 60)

(metro-tick!)

(do
  (defn metro-tick!
    []
    (let [cycl (*clock*)]
      (event :note :instrument kick :beat cycl :clock *clock*)
      ;; (apply-at (*clock* (+ cycl 1/2)) #(kick2))
      (event :note :instrument kick2 :start-time (*clock* (+ cycl 1/2)))
      ;; (event :note :instrument kick2 :beat (+ cycl 1/2) :clock *clock*)
      (apply-by (*clock* (inc cycl)) #'metro-tick!))))

(defn metro-tick! [])
