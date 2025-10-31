(ns cycl.play
  {:clj-kondo/ignore true}
  (:require
   [cycl.pattern :as p]
   [cycl.events :as e]
   [cycl.merge :as m]
   [cycl.util :refer [toggle!] :as u]
   [cycl.core :as c :refer [start! shutdown! o once sh! pause! now!]]
   [cycl.ops :refer :all]
   [cycl.dirt :refer [connect-dirt]]
   [clojure.pprint :refer [print-table]]
   [overtone.at-at :refer [now]]
   [cycl.music :as mu]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;

(c/set-cps! 1)

(connect-dirt)
;; Play a pattern once immediately
(now! (s :hh :sd))

;; Loop a pattern on an layer/output
(o 0 (s :bd))

;; Nothing? Start the clock.

(start!)

;; Clear the o 0 pattern
(o 0)
;; or
(o 0 nil)
;; or
(o 0 [])
;; Re-eval above to start it again

;; Play once in time
(once (s :hh :sd))

;; Notice the difference
(now! (s :hh :sd))


;; Modify the pattern
(o 0 (s :bd :sd))
(o 0 (s :bd [:hh :sd]))
(o 0 (s :bd (x 4 :sd)))
(o 0)


;; Add a pattern on another layer
(now! (s :supersaw) (nt :c :c# :d :d#))
(o 1 (s :superpiano) (nt :c :c# :d :d#))
(o 1)
(o 1 (s :piano) (n 4) (nt :c :eb :g :b))
(o 1 (s :piano) (n 4) (nt #{:c :eb :g :b}))
(o 1 (s :piano) (n 3) (nt (mu/chord :cm7)))
(o 1 (s :piano) (n 3) (nt (set (mu/chord :cm7))))
(o 1 (s :piano) (n 3) (nt (cyc (rep 2 (mu/chord :cm :incl 1))
                               (rep 2 (mu/chord :fm :incl 1))
                               (rep 2 (mu/chord :g7)))))
(o 1)

(now! (s :supermandolin) (jux rev (nt (mu/scale :c :minor :o 2 :incl 1))))
(once (s :piano) (n 3) (nt (set (mu/chord :cm :incl 1))))

(now! (s :piano) (n 3) (s| (nt #{:c :e} :d :e)))


(o 2 (nt (->> (mu/scale :c :minor) (slow 2))) (s :supermandolin) (room 2)) ;; TODO: Timing is wrong
(o 2)

(evts
 (->> (mu/scale :c :minor) (slow 2)))

(c/start!)
(c/clear-pattern!)

(o 2 nil)

(once (s (cyc :bd :sd :bd :sd)))

(o 0 (s :bd :sd :bd :sd))
(o 0)



(o 0 (s :piano) (n 3) (mnt (range 4)) (pan 1))

(o 0)

(->> (x 1 :bd :hh :sd :hh) s (o 1))
(o 1)


(once
 (s|
  (nt 60)
  (nt 63)
  (nt 67)
  (s :supermandolin)))


(once
 (s| (nt 60)
     (nt 70)
     (nt 63)
     (s :supermandolin)))


(o 0 (s (rep 8 :supermandolin)) (nt (+| sin (amp 60 72))))
(o 0 (s (rep 8 :supermandolin)) (nt (+| cos (amp 60 72))))
(o 0 (s (rep 8 :supermandolin)) (nt (+| saw (amp 60 72))))
(o 0 (s (rep 8 :supermandolin)) (nt (+| tri (amp 60 72))))
(o 0 (s (rep 8 :supermandolin)) (nt (+| [0 2 3 4] (mu/chord-nth :cm)))) ;; TODO: How to use sets
(o 0 (s (rep 8 :supermandolin)) (nt (+| [0 2 3 4] (mu/scale-nth :c :minor))))
(o 0 (s (rep 8 :supermandolin)) (nt (+| (cyc :cm :em :fM) mu/chord))) ;; TODO: Why doesn't cyc work

(o 0 (nt (x 2 :c :a :f :e)) (n 4) (s :piano) (pan sin))
(o 0)

(c/shutdown!)


(o 1
 (s (slow 2 (rep 8 :bd)))
 (pan (slow 2 (rep 8 0 1))))

(o 1)


(once
 (s| (nt :c) (nt :e) (nt :g) (s :supermandolin) (s :supersaw)))

(o 3 (->> (nt (mu/chord :cm7 :o 3 :incl 1)) ) (s :supermandolin))
(o 3)

(once (+| (nt :c2 :eb3 :g3 [:bb3 :c4])) (s :superpiano))
(once (+| (nt :c2 :eb3 :g3 #{:bb3 :c4})) (s :superpiano))

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
(o 3 (n (x 3 (irand 10))) (s :numbers))
(o 3 (m| (n 1) (s :numbers)))
(o 3)

(->> :numbers s (m| (n 2)) (o 3))

;; TODO
(o 3)
(o 3 (s :numbers) (n (irand 10)))
(o 3 (s :numbers) (->> 5 irand (x 4) n))

; d4 $ s "cp cp cp"

(o 1 (s :cp :cp :cp))
(o 1)

; d1 $ sound "drum" |+| n "1 2 3"

(o 1 (s :drum) (n 1 2 3)) ;; Implicit
(o 1 (s| (s :drum) (n 1 2 3))) ;; Explicit
(->> [1 2 3] n (|> (s :drum)) (o 1)) ;; Thread


;; d1 $ sound "drum" |+| n "2 3" |+| n "4 5 6"

(o 1 (s :drum) (n 2 3) (n 4 5 6))

;; d1 $ sound "bd*8" # pan sine

(o 1 (s (x 8 :bd)) (pan (x 4 sin))) ;; TODO: Partial periods vs repetition?
(o 1)
(o 0)
(c/clear-pattern!)


;; d1 $ sound "bd*8" # pan cosine # speed (sine + 0.5)

(o 1 (s (x 8 :bd)) (pan cos) (speed sin) (speed 0.5))
(o 1 (s (x 8 :bd)) (pan cos) (speed (+| sin 0.5)))
(o 1 (s (x 8 :bd)) (pan cos) (speed (+| sin (amp 0.5 1.5))))


;; d1 $ sound "bd*8" # pan (cat [square, sine])

(o 1 (s (x 8 :bd)) (pan square sin))

;; n("0 2 4 <[6,8] [7,9]>").scale("C:minor").sound("piano")
;; TODO: make this work.


(o 1)
;; if `scale-nth` is an fn
(o 1
 (a| (nt (a| [0 2 4 (cyc [6 8] [7 9])] (mu/scale-nth :c4 :minor))) (n 3) (s :piano))) ;; TODO: scale-nth does what you think

;; if `scale-nth` is a op -- variation of `n`, but that takes an arg-pat and applies nth
(m| (n (scale-nth :cm4 0 2 4 (cyc [6 8] [7 9]))) (s :piano)) ;; TODO?

;; #{ScaleNthOp ScaleNthOp* ChordNthOp ChordNthOp*}

(shutdown!)
;; Types

;; op :: any => Operatic
;; patop :: Operatic any => Operatic
;;
