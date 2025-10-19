(ns cyclops.play
  {:clj-kondo/ignore true}
  (:require
   [cyclops.pattern :as p]
   [cyclops.events :as e]
   [cyclops.merge :as m]
   [cyclops.util :refer [toggle!] :as u]
   [cyclops.core :as c :refer [o once sh! pause! now!]]
   [cyclops.ops :refer :all]
   [clojure.pprint :refer [print-table]]
   [overtone.at-at :refer [now]]
   [cyclops.music :as mu]))



(swap! c/defaults assoc :target :dirt)

(c/speak!)
;; (mixer/boot-server-and-mixer)
(c/start!)
(c/shutdown!)
(c/metro)

(mu/scale :c :minor)

(o 2 (+| (n (->> (mu/scale :c :minor :o 2 :incl 2) (slow 2))) (s :supermandolin) (room 2)))

(c/start!)
(c/clear-pattern!)
(o 2 (s :bd))
(once (s :hh :sd))
(now! (s :hh :sd))

(o 2 nil)

(once (s (cyc :bd :sd :bd :sd)))

(o 0 (s :bd :sd :bd :sd))
(o 0 nil)

(once
 (s|
  (+| (s :piano) (n (range 60 64)) (pan 0))
  (+| (s :piano) (rev (n (range 60 64))) (pan 1))))


(evts
 (n (->> [1 2 3] (slow 2))))


(evts
 (rev (s :superpiano)))

(o 0
   (+| (s :piano) (n (range 60 64)) (pan 1)))

(o 0 nil)

(once
 (s| (+| (n 10) (s :) (pan 0)) (+| (n 20) (s :superpiano) (pan 1))))

(evts
 (+| (range 10) (range 10)))


(reset! c/cps 1) ;; TODO: Why doesn't this work.

(pause!)
(->> (x 1 :bd :hh :sd :hh) s (o 1))

(e/events
 (f| Math/round
     (n 2.5 3.2 4.1)
     #_[(fn [_ v] (Math/round v))]))

(->> [2.5 3.2 4.1] (map inc) n events)

(map e/realize
     (events
      (n (fn [v _] (Math/floor v)))
      true))

(toggle! c/verbose)
(toggle! c/debug-osc)
(c/init-client!)
(c/shutdown!)

(once
 (n 60)
 (n 63)
 (n 67)
 (s :supermandolin))


(e/events
 (f| vector
     (n 60)
     (n 70)
     (n 63)
     (s :supermandolin)))


(o 0 (s (slow 3 (rep 8 :supermandolin ))) (n (e/sin 60 72 3))) ;; TODO: Why does this stutter?
(c/start!)
(pause!)


(once (n (mu/cycle-chord :cm7)) (s :supermandolin))

(sh!)
(speak!)

(cyc|ops)

(e/events (+| (n (slow* 2 (rep* 16 (e/sin 60 72)))) (s* :supersaw)))

(p/process-pattern (slow 2 (rep 16 (e/sin 60 72))))

(c/send-dirt {:s :supermandolin :n 66})


(o 0 (|+| (n (sin 60 70) (sin 60 70) (sin 60 70) (sin 60 70)) (s :supersaw)))


(e/slice
 (|+| (n* (e/sin 60 70) (e/sin 60 70) (e/sin 60 70) (e/sin 60 70)) (s* :supersaw) (legato* 1))
 (e/tc 0 1)
 {:realize? true})

(p/process-pattern
 (rep 4 [(e/sin 60 70)]))


(c/start!)
(c/pause!)

(toggle! c/verbose)
(toggle! c/sh)

(c/continue!)
(c/pause!)

(@c/layers 0)

(c/start!)
(c/sh!)
(c/speak!)

(reset! c/cps 1) ;; TODO: Why does changing this break stuff?

(o 0 (n (x 2 :c :a :f :e)) (s :superpiano) (pan (sin)))
(c/shutdown!)


(o 1
 (s (slow 2 (rep 8 :bd)))
 (pan (slow 2 (rep 8 0 1))))



(once
 (s| (n :c) (n :e) (n :g) (s :supermandolin) (s :supersaw)))


(c/restart!)

(o 3 (->> (n (chord :cm7 :o 3 :incl 1)) ) (s :supermandolin))

(once (+| (n :c2 :eb3 :g3 [:bb3 :c4])) (s :superpiano))

(slow 0.5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; d1 $ s "bd ~ bd ~"

(o 1 (s :bd nil :bd nil))
(o 1 (s (rep 2 :bd)))
(->> [:bd :- :bd :-] s (o 1))
(->> :bd (rep 2) s (o 1))

;; d2 $ s "[~ hh]*2"

(o 2 (s (rep 2 [:- :hh])))
(->> [:- :hh] (rep 2) s (o 2))


;; d3 $ s "numbers:1"
(o 3 (+| (n (fit (range 9))) (s :numbers)))
(c/restart!)
(o 3 (n 1) (s :numbers))
(o 3 (m| (n 1) (s :numbers)))

(->> :numbers s (m| (n 1)) (o 3))

;; TODO
(o 3 (s :numbers))

; d4 $ s "cp cp cp"

(o 1 (s :cp :cp :cp))

; d1 $ sound "drum" |+| n "1 2 3"

(o 1 (s :drum) (n 1 2 3)) ;; Implicit
(o 1 (s| (s :drum) (n 1 2 3))) ;; Explicit
(->> [1 2 3] n (m| (s :drum)) (o 1)) ;; Thread


;; d1 $ sound "drum" |+| n "2 3" |+| n "4 5 6"

(o 1 (s :drum) (n 2 3) (n 4 5 6))

;; d1 $ sound "bd*8" # pan sine

(o 1 (s (x 4 :bd)) (pan (sin 0 1 1/4))) ;; TODO: Just pass the var? i.e., (pan sin)

(c/restart!)
(c/shutdown!)

;; d1 $ sound "bd*8" # pan cosine # speed (sine + 0.5)

(o 1 (s (x 8 :bd)) (pan cosine) (speed (sin)) (speed 0.5)) ;; TODO: How does speed work?


;; d1 $ sound "bd*8" # pan (cat [square, sine])

(o 1 (s (x 8 :bd)) (pan (square) (sin)))

;; n("0 2 4 <[6,8] [7,9]>").scale("C:minor").sound("piano")
;; TODO: make this work.


;; if `scale-nth` is an fn
(apply| (n 0 2 4 (cyc [6 8] [7 9])) (n (scale-nth :cm4)) (s :piano)) ;; TODO: scale-nth does what you think

;; if `scale-nth` is a op -- variation of `n`, but that takes an arg-pat and applies nth
(m| (n (scale-nth :cm4 0 2 4 (cyc [6 8] [7 9]))) (s :piano))

;; Types

;; op :: any => Operatic
;; patop :: Operatic any => Operatic
;;
