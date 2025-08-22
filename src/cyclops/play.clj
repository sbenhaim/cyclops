(ns cyclops.play
  (:require
   [cyclops.pattern :refer [cyc cyc*] :as p]
   [cyclops.control :refer [n s s* n*]]
   [cyclops.events :as e]
   [cyclops.merge :as m :refer [|+|]]
   [cyclops.util :refer [toggle!]]
   [cyclops.core :refer [o] :as c]))


(time
 (-> [:bd]
     s))


(o 0 (|+| (n* :c :a :f :e) (s* :supermandolin)))

e/offset-slice 1/4
(e/slice
 (|+| (s* :bd))
 (e/tc 0 1/4))

(time
 (doall
  (|+| (n* 0 2 4 5) (s* :supermandolin))))

(c/start!)

(toggle! c/verbose)
(toggle! c/sh)

(c/pause!)
(c/continue!)
(c/pause!)

(@c/layers 0)
