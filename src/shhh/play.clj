(ns shhh.play
  (:require [shhh.core :refer [start! restart! play! pause! continue! sh! speak! set-pattern! verbose cps freq-s o loops] :as sh]
            [shhh.music
             :refer [cycle-chord note cycle-scale]
             :rename {cycle-chord cc note n cycle-scale cs}
             :as m]
            [shhh.looping :as l]
            [shhh.pattern
             :refer [fit times cycle slow splice rep elongate prob euclid stack pick]
             :rename {fit f times x cycle cyc slow slw splice spl elongate el prob ? euclid euc stack s}
             :as p]
            [shhh.util :refer [cycle-n toggle!]]))


(sh/restart!)
(toggle! verbose)
(toggle! sh/debug-osc)

(do
  (start!))

(start!)
(restart!)
(sh/clear-pattern!)
(reset! loops (l/pattern->loops (apply ? 0.9 (repeat 16 :hh))))
(pause!)
(sh/continue!)

(sh/clear-pattern!)
(reset! cps 1)

(swap! sh/debug-osc not)
(o 0 (times 16 :bd))

(pause!)


(l/slice
 (first @sh/loops)
 0
 2)

(-> @loops
    first
    second
    last
    identity
    sh/create-event
    )
