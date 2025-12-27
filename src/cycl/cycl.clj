(ns cycl.cycl
  "Seq of events. Assumed sorted."
  (:require
   [cycl.event :as e]
   [cycl.util :as u]))


(defn sort-cycl
  [cycl]
  (sort e/event-compare cycl))


(defn cycl?
  [v]
  (and (coll? v)
       (every? e/event? v)))

(comment
  (cycl? [{:start 0 :period 1 :length 5}])
  (cycl? [{:start 0 :period 1}]))


(defn slice-starts
  [cycl from length]
  (->> cycl
       (drop-while #(< (e/start %) from))
       (take-while #(< (e/start %) (+ from length)))))


(defn slice-active
  ;; TODO: Whole/part
  [cycl from end]
  (->> cycl
       (drop-while #(<= (e/end %) from))
       (take-while #(< (e/start %) end))))


(defn offset [cycl amount]
  (map #(update % :start (partial + amount)) cycl))


(defn start [cycl]
  (-> cycl first e/start))


(defn end [cycl]
  (apply max (map e/end cycl)))


(defn length [cycl]
  (- (end cycl) (start cycl)))


(defn guess-period [cycl]
  (-> cycl end Math/ceil long))


(defn shape
  [cycl]
  ((juxt start length) cycl))


(defn scale
  "Slows a cycle by a factor of x (or speeds it up if (< x 1)."
  [cycl x]
  (let [xer #(* x %)]
    (map (fn [evt]
           (-> evt
               (update :start xer)
               (update :length xer)))
         cycl)))


(defn translate
  [cycl beg len]
  (let [orig-start (start cycl)
        orig-len   (length cycl)
        factor     (/ len orig-len)]
    (->
     (for [evt cycl]
       (-> evt
           (update :start #(+ beg (* (- % orig-start) factor)))
           (update :length #(* % factor)))))))


(defn realize-cycl
  [cycl ctx]
  (map (u/p2 e/realize-event ctx) cycl))
