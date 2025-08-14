(ns cyclops.play
  (:require [cyclops.core :refer [start! restart! play! pause! continue! sh! speak! set-pattern! verbose cps freq-s o loops] :as sh]
            [cyclops.music
             :refer [cycle-chord note cycle-scale]
             :rename {cycle-chord cc note nt cycle-scale cs}
             :as m]
            [cyclops.ops :refer :all]
            [cyclops.pattern
             :refer [|+|]]
            [cyclops.util :refer [cycle-n toggle!]]
            [cyclops.pattern :as pat]))


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


(start!)
(comment (o 0 (|+| (n (cs :c :minor :o 2)) (s* :supermandolin) (pan (range 0 1 0.1)) (legato* 8))))

(sh!)
(speak!)
(pause!)


(comment
  (let [a (n* :c :c :c :c)
        b (n* 0 3 5)
        b (pat/->SinLoop 0 10 2)]
    (pat/merge-loops (partial pat/merge-both +) a b :structure-from :both)))



(let [a (s* :saw :tri)
      b (n* 60 62 64)]
  (pat/merge-loops (partial pat/merge-left +) a b :structure-from :left))
