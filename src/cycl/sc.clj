(ns cycl.sc
  (:require [overtone.sc.server :as server]
            [overtone.sc.sclang :as lang]
            [babashka.process :as p]
            [clojure.java.io :as io]))


(defn start-sclang []
  (let [scl-path (lang/sclang-path)
        startup-path (-> "sc/startup.scd" io/file .getAbsolutePath)]
    (p/process {:out *out* :err *err*}
             scl-path
             startup-path)))


(def sclang-process (atom nil))


(defn boot! []
  (server/boot-server)
  (reset! sclang-process (start-sclang)))


(defn shutdown! []
  (server/kill-server)
  (when @sclang-process
    (p/destroy sclang-process)))


(comment
  (boot!)
  (start-sclang)
  (shutdown!)
  )
