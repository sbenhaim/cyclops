(ns cyclops.play
  (:require
   [cyclops.pattern :as p]
   [cyclops.control :as ctrl]
   [cyclops.events :as e]
   [cyclops.merge :as m :refer [|+|]]
   [cyclops.util :refer [toggle!]]
   [cyclops.core :refer [o] :as c]
   [cyclops.ops :refer :all]
   [overtone.at-at :refer [now]]))



(toggle! c/verbose)
(toggle! c/debug-osc)
(c/start!)
(c/shutdown!)

(o 0 (|+| (n* (rep* 16 (e/sin 60 72)))))
(c/tick!)

(o 0 (|+| (n (slow* 1/2 (rep* 16 (e/sin 60 72)))) (s* :supermandolin)))

(e/events (|+| (n (slow* 2 (rep* 16 (e/sin 60 72)))) (s* :supersaw)))

(p/process-pattern (slow* 2 (rep* 16 (e/sin 60 72))))

(c/send-dirt {:s :supermandolin :n 66})

;; Todo

(o 0 (|+| (n* (e/sin 60 70) (e/sin 60 70) (e/sin 60 70) (e/sin 60 70)) (s* :supersaw)))


(e/slice
 (|+| (n* (e/sin 60 70) (e/sin 60 70) (e/sin 60 70) (e/sin 60 70)) (s* :supersaw) (legato* 1))
 (e/tc 0 1)
 {:realize? true})

(p/process-pattern
 (rep 4 [(e/sin 60 70)]))

;; TODO: Can't just execute fns on merge. Perhaps on `slice`?

(c/start!)
(c/pause!)

(toggle! c/verbose)
(toggle! c/sh)

(c/continue!)
(c/pause!)

(@c/layers 0)
