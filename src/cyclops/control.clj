(ns cyclops.control
  (:require
   [cyclops.events :as e]
   [cyclops.music :as m]
   [cyclops.pattern :as pat]))


(defrecord Control [cycle key value-tx]
  e/Cyclic
  (period [_] (e/period cycle))
  (events [this] (e/slice this 0 (e/period this) {}))
  (slice [this from to] (e/slice this from to {}))
  (slice [_ from to opts]
    (let [evts (e/slice cycle from to opts)]
      (map (fn [e]
             (-> e
                 (assoc key (value-tx (:value e)))
                 (dissoc :value)))
           evts))))


(defmacro defcontrol
    {:clj-kondo/lint-as :clojure.core/defn :clj-kondo/ignore true}
    ([control doc value-tx]
     `(defcontrol ~control ~doc ~(keyword control) ~value-tx))
    ([control doc param value-tx]
     `(do
        (defn ~control ~doc [pat#]
          (->Control (pat/process-pattern pat#) ~param ~value-tx))
        (defn ~(symbol (str control "*")) ~doc [& pat#]
          (~control pat#)))))


(defn rest? [v]
  (or (nil? v) (#{:- "~"} v)))


(defn parse-n
  [n]
  (cond
    (rest? n)    nil
    (keyword? n) (m/note n)
    (string? n)  (m/note n)
    (number? n)  (float n)
    :else        n))


(defcontrol n "Numbers and notes." :n parse-n)


(defn parse-s
  [s]
  (cond
    (rest? s)    nil
    (keyword? s) (name s)
    :else        s))


(defcontrol s "Samples and synths" parse-s)

(defcontrol pan "Left 0.0, Right 1.0" float)

(defcontrol vowel ":a :e :i :o :u" name)

(defcontrol room "Reverb room size" float)

(defcontrol size "Reverb size" float)

(defcontrol dry "Reverb dry" float)

(defcontrol legato "Play note for `n` segments, then cut." float)

(defcontrol orbit "Common effects orbit (channel/track) in Superdirt" float)




(comment
  (let [nums (e/period (n [:a :b :c]))
        ss (s [:bd :sd :sd])]))
