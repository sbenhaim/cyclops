(ns shhh.music
  (:require [overtone.music.pitch :as m]))


(defn note
  ([n]
   (let [n      (name n)
         o (re-find #"(.*?)(\d)" n)]
     (if o (note (second o) (-> o last parse-long))
        (note n 4))))
  ([n o]
   (m/note (str (name n) o))))
