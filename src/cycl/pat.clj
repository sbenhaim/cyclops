(ns cycl.pat
  "Deferred pattern implementation using Records + Protocols.
   Patterns are data that produce events at query time via the Sliceable protocol."
  (:require [cycl.event :as e]
            [cycl.util :as u]))

;; Core protocol

(defprotocol Sliceable
  (slice [this from length ctx]
    "Query pattern for events in [from, from+length). Returns seq of Events.")
  (period [this]
    "Base period of the pattern.")
  (weight [this]
    "Weight for relative spacing in Fit. Default is 1."))

;; Context for queries - carries cycle-num for deterministic randomness

(defn make-ctx
  ([] {:cycle-num 0})
  ([cycle-num] {:cycle-num cycle-num}))

;; Helper: events for cycles touched by [from, from+length)

(defn- touched-cycles [from length]
  (let [to (+ from length)
        start-cycle (long (Math/floor from))
        end-cycle (long (Math/ceil to))]
    (range start-cycle end-cycle)))

;; Atom pattern - single value per cycle

(defrecord Pure [value]
  Sliceable
  (slice [_ from length _ctx]
    (for [c (touched-cycles from length)
          :let [evt-start c
                evt-end (+ c 1)]
          :when (and (< evt-start (+ from length))
                     (> evt-end from))]
      (e/->event value c 1 1)))
  (period [_] 1)
  (weight [_] 1))

;; Sequential composition - fit patterns into one cycle

(defrecord Fit [patterns]
  Sliceable
  (slice [_ from length ctx]
    (let [weights (mapv weight patterns)
          n (reduce + weights)
          offsets (reductions + 0 weights)]
      (mapcat
       (fn [pat w offset]
         (let [slot-start (/ offset n)
               slot-width (/ w n)
               ;; Transform query to pattern's local time
               ;; local = (global - slot-start) / slot-width
               local-from (/ (- from slot-start) slot-width)
               local-to (/ (- (+ from length) slot-start) slot-width)
               local-length (- local-to local-from)]
           (when (and (< local-from (period pat))
                      (> local-to 0))
             (->> (slice pat
                         (max 0 local-from)
                         (min (period pat) local-length)
                         ctx)
                  (map (fn [evt]
                         (-> evt
                             (update :start #(+ slot-start (* % slot-width)))
                             (update :length #(* % slot-width)))))))))
       patterns weights offsets)))
  (period [_] 1)
  (weight [_] 1))

;; Cycling through patterns - one per cycle

(defrecord Cyc [patterns]
  Sliceable
  (slice [_ from length ctx]
    (let [weights (mapv weight patterns)
          n (reduce + weights)
          offsets (reductions + 0 weights)
          to (+ from length)
          ;; Find all period instances that overlap [from, to)
          start-period (long (Math/floor (/ from n)))
          end-period (long (Math/ceil (/ to n)))]
      (mapcat
       (fn [period-num]
         (mapcat
          (fn [slot-idx]
            (let [pat (nth patterns slot-idx)
                  w (nth weights slot-idx)
                  slot-offset (nth offsets slot-idx)
                  ;; Global position of this slot
                  slot-start (+ (* period-num n) slot-offset)
                  slot-end (+ slot-start w)]
              ;; Only process if slot overlaps query window
              (when (and (< slot-start to) (> slot-end from))
                (let [;; Query window relative to slot
                      local-from (max 0 (/ (- from slot-start) w))
                      local-to (min 1 (/ (- to slot-start) w))
                      local-length (- local-to local-from)]
                  (when (pos? local-length)
                    (->> (slice pat local-from local-length ctx)
                         (map (fn [evt]
                                (-> evt
                                    (update :start #(+ slot-start (* % w)))
                                    (update :length #(* % w))
                                    (assoc :period n))))))))))
          (range (count patterns))))
       (range start-period end-period))))
  (period [_] (reduce + (mapv weight patterns)))
  (weight [_] 1))

;; Repeat n times compressed

(defrecord Times [n pattern]
  Sliceable
  (slice [_ from length ctx]
    (let [p (period pattern)]
      (mapcat
       (fn [i]
         (let [slot-start (/ i n)
               slot-width (/ 1 n)
               ;; Transform to local time
               local-from (* n (- from slot-start))
               local-to (* n (- (+ from length) slot-start))
               local-length (- local-to local-from)]
           (when (and (< local-from p) (> local-to 0))
             (->> (slice pattern
                         (max 0 local-from)
                         (min p local-length)
                         ctx)
                  (map (fn [evt]
                         (-> evt
                             (update :start #(+ slot-start (/ % n)))
                             (update :length #(/ % n)))))))))
       (range n))))
  (period [_] (period pattern))
  (weight [_] 1))

;; Tempo scaling

(defrecord Slow [factor pattern]
  Sliceable
  (slice [_ from length ctx]
    (let [local-from (/ from factor)
          local-length (/ length factor)]
      (->> (slice pattern local-from local-length ctx)
           (map (fn [evt]
                  (-> evt
                      (update :start #(* % factor))
                      (update :length #(* % factor))
                      (update :period #(* % factor))))))))
  (period [_] (* factor (period pattern)))
  (weight [_] 1))

;; Weight for relative spacing (used by Fit/Cyc)

(defrecord Elongate [w pattern]
  Sliceable
  (slice [_ from length ctx]
    (slice pattern from length ctx))
  (period [_] (period pattern))
  (weight [_] w))

;; Wrapper for eager event sequences (backward compat)

(defrecord Eager [events]
  Sliceable
  (slice [_ from length _ctx]
    (let [to (+ from length)]
      (filter (fn [evt]
                (and (< (:start evt) to)
                     (> (+ (:start evt) (:length evt)) from)))
              events)))
  (period [_]
    (if (seq events)
      (apply max (map :period events))
      1))
  (weight [_] 1))

;; Pattern merging - apply op from arg-pat to val-pat

(defrecord WithPat [op arg-pat val-pat]
  Sliceable
  (slice [_ from length ctx]
    (let [arg-evts (slice arg-pat from length ctx)]
      (mapcat
       (fn [arg-evt]
         (let [arg-val (e/get-init arg-evt)
               arg-start (:start arg-evt)
               arg-len (:length arg-evt)
               ;; Query val-pat within arg-evt's span
               val-evts (slice val-pat arg-start arg-len ctx)]
           ;; Apply op: expects (op arg-value pattern) -> pattern
           ;; But we have events, so we apply op conceptually
           (slice (op arg-val (->Eager val-evts)) from length ctx)))
       arg-evts)))
  (period [_] (u/lcm (period arg-pat) (period val-pat)))
  (weight [_] 1))

;; Probabilistic - degrade with cycle-seeded randomness

(defrecord Degrade [prob pattern]
  Sliceable
  (slice [_ from length ctx]
    (let [seed (hash [(:cycle-num ctx) from])
          rng (java.util.Random. seed)]
      (->> (slice pattern from length ctx)
           (filter (fn [_] (<= (.nextDouble rng) prob))))))
  (period [_] (period pattern))
  (weight [_] 1))

;; Coercion

(defn ->pat [x]
  (cond
    (satisfies? Sliceable x) x
    (sequential? x) (if (every? e/event? x)
                      (->Eager x)
                      (->Fit (mapv ->pat x)))
    :else (->Pure x)))

;; User API

(defn fit [& xs]
  (->Fit (mapv ->pat xs)))

(defn cyc [& xs]
  (->Cyc (mapv ->pat xs)))

(defn x [n p]
  (->Times n (->pat p)))

(defn el [w p]
  (->Elongate w (->pat p)))

(defn rep [n p]
  (el n (x n p)))

(defn slow [f p]
  (->Slow f (->pat p)))

(defn speed [f p]
  (->Slow (/ 1 f) (->pat p)))

(defn degrade [prob p]
  (->Degrade prob (->pat p)))

(defn with-pat [op arg-pat val-pat]
  (->WithPat op (->pat arg-pat) (->pat val-pat)))

;; REPL helpers

(defn materialize
  "Force pattern evaluation for n cycles. For REPL inspection."
  ([pat] (materialize pat 1))
  ([pat n]
   (vec (slice (->pat pat) 0 n (make-ctx 0)))))

(comment
  ;; Examples
  (materialize :a)
  (materialize (fit :a :b :c))
  (materialize (cyc :a :b) 4)
  (materialize (x 3 :a))
  (materialize (fit (el 2 :a) :b))
  (materialize (slow 2 (fit :a :b)))
  (materialize (degrade 0.5 (fit :a :b :c :d)) 1))
