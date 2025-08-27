(ns cyclops.play
  {:clj-kondo/ignore true}
  (:require
   [cyclops.pattern :as p]
   [cyclops.events :as e]
   [cyclops.music :as m]
   [cyclops.util :refer [toggle!] :as u]
   [cyclops.core :refer [o once sh! pause!] :as c]
   [cyclops.ops :refer :all]
   [overtone.at-at :refer [now]]
   [overtone.studio.mixer :as mixer]))



;; (mixer/boot-server-and-mixer)
(c/start!)

(once (s :bd))

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


(o 0 (|+| (n* (e/sin 60 70) (e/sin 60 70) (e/sin 60 70) (e/sin 60 70)) (s* :supersaw)))


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

(o 0 (n (x 2 :c :a :f :e)) (s :superpiano) (pan (e/sin)))
(c/shutdown!)


(o 1
 (s (slow 2 (rep 8 :bd)))
 (pan (slow 2 (rep 8 0 1))))

(e/slice
 (pan
  (e/sin 0 1 1))
 0 1 {:realize? true})


(e/cycle-events 1 (p/process-pattern [(e/sin 0 1 1)]))



(once
 (|s|  (n :c) (n :e) (n :g) (s :supermandolin) (s :supersaw)))


(e/events
 (|s| (n :c) (n :e) (n :g) (s :supersaw) (s :supermandolin))
 true)
