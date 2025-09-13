(ns cyclops.pattern
  (:require
   [cyclops.events :as e]
   [cyclops.music :as m]
   [cyclops.util :as u :refer [cycle-n]]
   [cyclops.merge :as merge]))

;; Ops

(defrecord OpContext
    [period         ;; Number of cycles before looping
     segment-length ;; How much segments does it occupy per cycle
     spacing        ;; How many segments between events
     start          ;; Where it sits in the pattern
     ])


(defprotocol Weighty
  (weigh [this]))


(defn sum-weights
  "Adds the weights of all children of an op recursively."
  [stuff]
  (reduce + (map weigh stuff)))


(extend-type java.lang.Object
  Weighty
  (weigh [_] 1))


(defprotocol Operatic
  (operate [this ctx]))


(def base-context (->OpContext 1 1 1 0))


(defn ->cycle
  "Takes the base of a raw, nested op structure and turns it into a timed event Cycle."
  [op]
  (-> op
      (operate base-context)
      e/collapse-events))


(extend-type cyclops.pattern.Operatic
  e/DoYouRealize?
  (realize [this ctx] (-> this ->cycle (e/realize ctx))))


(defn op?
  [e]
  (satisfies? Operatic e))


;; TODO: Trampoline with operate?
(defn apply-timing
  "Given a collection of events and a timing `Context`, recursively schedules
  the events honoring weights."
  [pat ^OpContext {:keys [period spacing segment-length start] :as context}]
  (loop [pat pat start start events []]
    (if-not
        (seq pat) events
        (let [[child & pat] pat
              weight        (weigh child) ;; How many segments to occupy
              length        (* weight segment-length)
              ctx           (assoc context
                                   :start start
                                   :segment-length length) ;; Context for children operations
              next-start    (+ start (* weight spacing))]
          (recur pat next-start
                 (conj events
                       (cond
                         (op? child)      (operate child ctx) ;; If child is an op, apply with inherited context
                         (e/event? child) (assoc child :start start :length length :period period)
                         :else            (e/->event child ;; Otherwise, add timing information to the event. Event keys can override (e.g., `length`).
                                          start
                                          length
                                          period))))))))



;; TODO: Should be a way to only define update-ctx since that's the only thing
;; that distinguishes current ops.

#_(defmacro defop
  {:clj-kondo/lint-as :clojure.core/defn :clj-kondo/ignore true}
  [op-name doc args & methods]
  (let [op-kw     (keyword op-name)
        operate   (first (filter #(= 'operate (first %)) methods))
        weigh     (first (filter #(= 'weigh (first %)) methods))
        op-impl   (first (filter #(= op-name (first %)) methods))
        op-args   (vec (remove #{'&} (butlast args)))
        splatted? (some #{'&} args)
        pat-arg   (if splatted? `(u/smart-splat ~(last args)) (last args))]
    (assert operate "`operate`` must be defined for op.")
    `(let []
       (defmethod operate [Op ~op-kw]
         ~@(rest operate))
       ~@(when weigh
           `(defmethod weigh [Op ~op-kw]
              ~@(rest weigh)))
       ~(if op-impl
          `(defn ~op-name ~doc ~@(rest op-impl))
          `(defn ~op-name ~doc [~@args]
             (->Op ~op-kw ~op-args ~pat-arg))))))


(defn fit-children
  "Squeeze children into inherited segment. Spacing and segment length will be the same. Period remains unchanged."
  [children ^OpContext {:keys [segment-length] :as context}]
  (if-not
      (seq children) []
      (let [n              (sum-weights children)
            spacing        (/ segment-length n)
            segment-length spacing]
        (apply-timing children
                      (assoc context
                             :spacing spacing
                             :segment-length segment-length)))))


(extend-type clojure.lang.Sequential
  Operatic
  (operate [this ctx] (fit-children this ctx))
  e/DoYouRealize?
  (realize [this ctx] (e/realize (->cycle this) ctx)))


(defrecord FitOp [children]
  Operatic
  (operate [_ ctx] (operate children ctx)))




(defrecord TimesOp [n children]
  Operatic
  (operate [_this ctx]
    (fit-children (u/cycle-n n children) ctx)))



(defn splice
  [children ^OpContext {:keys [segment-length] :as context}]
  (let [n              (sum-weights children)
        segment-length (/ segment-length n)]
    (apply-timing children (assoc context
                                  :spacing segment-length
                                  :segment-length segment-length))))


(defrecord SpliceOp [children]
  Operatic
  (operate [_this ctx]
    (splice children ctx))
  Weighty
  (weigh [_] (sum-weights children)))


(defrecord RepOp [n children]
  Operatic
  (operate [_ ctx]
    (splice (cycle-n n children) ctx))
  Weighty
  (weigh [_] (* n (sum-weights children))))


;; Period Ops

(defn apply-slow
  "Stretch the children across `factor` segments by altering `period` and stretching `spacing` and `segment-length`."
  [x children {:keys [period segment-length] :as context}]
  (let [n              (sum-weights children)
        cycle-period   (* x period)
        spacing        (/ cycle-period n)
        segment-length (/ (* segment-length x) n)]
    (apply-timing children (assoc context
                                  :period cycle-period
                                  :spacing spacing
                                  :segment-length segment-length))))


(defrecord SlowOp [x children]
  Operatic
  (operate [_ ctx] (apply-slow x children ctx)))


(defn bjork
  ([ps os] (bjork ps os []))
  ([ps os res]
   (if (or (not (seq ps)) (not (seq os)))
     (let [step    (concat res ps os)
           [ps os] (split-with #(= (first step) %) step)]
       (if (<= (count os) 1)
         (flatten (concat ps os))
         (recur ps os [])))
     (recur (rest ps) (rest os)
            (conj res (concat (first ps) (first os)))))))


(defn basic
  "Only moves value from `:init` to `:param`"
  [param pat]
  (->> pat ->cycle (e/map-events #(e/reassoc-param % :init param))))


(defn pat-op
  [op-fn arg-pat pat]
  (let [param    (gensym)
        args     (basic param arg-pat)
        cycl     (->cycle pat)
        merge-fn (fn [e o] [(get-in o [:params param]) e])
        prepared (merge/merge-two merge-fn cycl args :structure-from :both)]
    (->SpliceOp
     (map (fn [[arg & evt]] (op-fn arg [evt]))
          (e/events prepared)))))


(defn ->op
  ([op children]
   (op (u/smart-splat children)))
  ([op argpat children]
   (let [kids (u/smart-splat children)
         args (u/ensure-vec argpat)]
     (pat-op op args kids))))



;; Controls

;; TODO: Does this work?
(defn ->ctrl
  [param value-tx pat]
  (let [cyc
        (->> pat
             ->cycle
             (e/map-events
              #(-> % (e/reassoc-param :init param (fn [v] (u/defer value-tx v))))))
        cyc]))




(defn rest? [v]
  (or (nil? v) (#{:- "~"} v)))


(defn parse-num
  [n]
  (if (coll? n) (mapv parse-num n)
      (cond
        (rest? n)    nil
        (keyword? n) (m/note n)
        (string? n)  (m/note n)
        (number? n)  (float n)
        :else        n)))


(comment
 (parse-num [:a :b]))


(defn parse-sound
  [s]
  (if (coll? s) (mapv parse-sound s)
      (cond
        (rest? s)    nil
        (keyword? s) (name s)
        :else        s)))
