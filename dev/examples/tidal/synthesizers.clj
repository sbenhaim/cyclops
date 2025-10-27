(ns examples.tidal.synthesizers
  (:require
   [cycl.pattern :as p]
   [cycl.events :as e]
   [cycl.merge :as m]
   [cycl.util :refer [toggle!] :as u]
   [cycl.core :as c :refer [start! shutdown! o once sh! pause! now!]]
   [cycl.music :as mu]
   [cycl.dirt]
   [cycl.ops :refer :all]))

(cycl.dirt/connect-dirt)

;; d1 $ n (slow 2 $ fmap (*7) $ run 8)
;;   # s "supergong"
;;   # decay "[1 0.2]/4"
;;   # voice "[0.5 0]/8"

(now! (n (slow 2 (map #(* 7 %) (range 8))))
      (s :supergong)
      (decay (slow 4 1 0.2))
      (voice (slow 8 0.5 0)))


;; d1 $ s "supertron"
;;   # octave 3
;;   # accelerate "0.2"

(now! (s :supertron)
      (octave 3)
      (accelerate 0.2))
