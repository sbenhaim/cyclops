(ns shhh.play
  (:require [shhh.core :refer
             [start!
              restart!
              play!
              pause!
              continue!
              sh!
              speak!
              set-pattern!
              verbose
              cps
              freq-s] :as sh]
            [shhh.music :refer [note]]
            [shhh.pattern :refer
             [fit
              times
              cycle
              slow
              splice
              rep
              elongate
              prob
              euclid
              stack
              rnd] :as p]
            [shhh.util :refer [cycle-n]]))


(swap! verbose not)

(do
  (start!)
  (play! (cycle :bd :sd)))

(play!)

(p/process-pattern (times 8 :sd))

(continue!)
(start!)

(shhh!)

(pause!)

(do
  (reset! cps 1)
  (set-pattern! [:bd (splice :sd :sd :sd)])
  (start!))


(play! [(cycle :bd :bd :cr) (splice :sd :sd :sd)])

(pause!)

(reset! verbose false)
(swap! sh/debug-osc not)

(sh/pos->s 1/4)
(continue!)

(play! (for [n (cycle-n 4 [:c :a :f :e])]
         {:s "supermandolin" :n (note n) :delta 2.0}))

(sh!)
