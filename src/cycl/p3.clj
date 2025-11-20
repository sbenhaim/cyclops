(ns cycl.p3
  (:require [cycl.events :as e]
            [cycl.util :as u]
            [cycl.merge :as merge]))


(defn ->cycl?
  [thing]
  (cond
    (e/cycl? thing)  thing
    (e/event? thing) [thing]
    :else            [(e/->event thing)]))


(defn encyclify
  [pattern]
  (map ->cycl? pattern))


(defn weigh
  [cycl]
  (or (-> cycl meta :weight) 1))


(defn weigh*
  [cycls]
  (reduce + (map weigh cycls)))


(defn scale-event
  [evt segment-start segment-len period]
  (let [start (:start evt)
        cycle (long start)
        frac  (- start cycle)]
    (-> evt
        (assoc :start (+ cycle segment-start (* frac segment-len)))
        (update :length #(* % segment-len))
        (update :period #(* % period)))))


(defn scale
  [x cycls]
  (let [weights (map weigh cycls)
        n       (reduce + weights)
        offsets (reductions + 0 weights)]
    (mapcat (fn [cycl weight offset]
              (let [segment-start (* offset x (/ n))
                    segment-len   (* weight x (/ n))]
                (map
                 (fn [evt]
                   (let [start (:start evt)
                         cycle (long start)
                         frac  (- start cycle)]
                     (-> evt
                         ;; Retain cycle pos, offset to pos in parent, scale original offset to enclosing segment
                         (assoc :start (+ cycle segment-start (* frac segment-len)))
                         ;; Scale length to new enclosing segment
                         (update :length #(* % segment-len))
                         ;; Update periods
                         (update :period #(* % x)))))
                 cycl)))
            cycls
            weights
            offsets)))


(comment
  (scale 1 (encyclify [:a :b :c :d]))
  (scale 1 (encyclify [:a :b (scale 1 (encyclify [:c :d]))]))
  (scale 1 (encyclify [:a :b (with-meta (scale 1 (encyclify [:c :d])) {:weight 2})]))
  (scale 2 (encyclify [:a :b (with-meta (scale 1 (encyclify [:c :d])) {:weight 2})]))
  (scale 1/2 (encyclify [:a :b (scale 1 (encyclify [:c :d]))])))


(defn spread
  [x cycls]
  (let [weights (map weigh cycls)
        n       (reduce + weights)
        offsets (reductions + 0 weights)]
    (mapcat (fn [cycl weight offset]
              (map
               (fn [evt]
                 (let [start (:start evt)
                       cycle (long start)
                       frac (- start cycle)
                       factor (/ x n)]
                   (-> evt
                       ;; Scale original cycle num by parent weight, add spread offset scaled by spreading factor, re-introduce fractional offset
                       (assoc :start (+ (* factor offset) (* cycle n) frac)) 
                       ;; Scale weight by spread factor ratio
                       (update :length #(* % factor weight))
                       ;; Scale period by x
                       (update :period #(* % x)))))
               cycl))
            cycls
            weights
            offsets)))


(comment
  (spread 4 (encyclify [:a :b :c :d]))
  (spread 3 (encyclify [:a :b :c :d]))
  (spread 2 (encyclify [:a :b :c :d]))
  (spread 3 (encyclify [:a :b (spread 2 (encyclify [:c :d]))]))
  (scale 2 (encyclify [:a :b (scale 2 (encyclify [:c :d]))]))
  (spread 4 (encyclify [:a :b (with-meta (scale 2 (encyclify [:c :d])) {:weight 2})]))
  (scale 2 (encyclify [:a :b (with-meta (scale 1 (encyclify [:c :d])) {:weight 2})]))
  (scale 1/2 (encyclify [:a :b (scale 1 (encyclify [:c :d]))])))



(defn fit
  [& pattern]
  (let [cycls (encyclify pattern)]
    (scale 1 cycls)))

(comment
  (fit :a :b :c)
  (fit :a (fit :b :c)))


(defn cyc
  [& pattern]
  (let [cycls (encyclify pattern)
        n (reduce + (map weigh cycls))]
    (spread n cycls)))

(comment
  (cyc :a :b :c)
  (cyc :a (cyc :b :c)))


(comment
  (fit :a (cyc :b :c))
  (cyc :a (fit :b :c))
  (fit (cyc :a :b) (cyc :c (fit :d :e)) :f)
  (cyc (fit :a :b) (cyc :c (fit :d :e)) :f))
