(ns cyclops.play
  (:require
   [cyclops.pattern :refer [cyc cyc* rep* rep] :as p]
   [cyclops.control :refer [n s s* n* legato*]]
   [cyclops.events :as e]
   [cyclops.merge :as m :refer [|+|]]
   [cyclops.util :refer [toggle!]]
   [cyclops.core :refer [o] :as c]))


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
