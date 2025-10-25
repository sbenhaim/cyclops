(ns scratch
  (:require [overtone.sc.server :as sc]
            [overtone.sc.sclang :as scl]
            [babashka.process :as proc :refer [shell process]]
            [cyclops.pattern :as p]))


(sc/connection-info)


(sc/boot-server)
(sc/kill-server)
(sh )

(def p
  (process {:out *out* :err *err*}
           "/Applications/SuperCollider.app/Contents/MacOS/sclang"
           "/Users/selah/code/cyclops/sc/startup.scd"))


(proc/destroy p)
