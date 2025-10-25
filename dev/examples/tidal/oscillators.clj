(ns examples.tidal.oscillators
  {:clj-kondo/ignore true}
  (:require
   [cyclops.pattern :as p]
   [cyclops.events :as e]
   [cyclops.merge :as m]
   [cyclops.util :refer [toggle!] :as u]
   [cyclops.core :as c :refer [start! shutdown! o once sh! pause! now!]]
   [cyclops.ops :refer :all]
   [clojure.pprint :refer [print-table]]
   [overtone.at-at :refer [now]]
   [cyclops.music :as mu]))


(start!)
(shutdown!)

(c/set-cps! 1/2)

;; d1 $ sound "bd*8" # pan sine
(now! (s (x 8 :bd)) (pan sin))

;; d1 $ sound "bd*8" # pan cosine # speed (sine + 0.5)
(now! (s (x 8 :bd)) (pan cos) (speed (+| sin 0.5)))

;; d1 $ sound "bd*8" # pan (cat [square, sine])
(now! (s (x 8 :bd)) (pan square sin))

;; d1 $ sound "bd*16" # speed (slow 2 $ range 0.5 2 tri)
(now! (s (x 16 :bd)) (speed (slow 2 (+| tri (amp 0.5 2)))))

;; d1 $ sound "bd*8" # pan (slow 2 saw)
(now! (s (x 8 :bd)) (pan (slow 2 saw)))

;; d1 $ sound "bd*8" # pan (slow 2 isaw)
(now! (s (x 8 :bd)) (pan (slow 2 isaw)))

;; d1 $ sound "bd*4" # pan (slow 4 $ smooth "0 1 0.5 1")
;; TODO: (now! (s (x 4 :bd)) (pan (slow 4 (smooth 0 1 0.5 1)))) ;; Smooth will need to be an op

;; d1 $ sound "bd*8" # pan rand
(now! (s (x 8 :bd)) (pan rand))


