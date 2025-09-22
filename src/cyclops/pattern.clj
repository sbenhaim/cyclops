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


(extend-protocol Weighty
  nil
  (weigh [_] 1)

  java.lang.Object
  (weigh [_] 1))


;; TODO: Add children to interface?
(defprotocol Operatic
  (operate [this ctx]))


(def base-context (->OpContext 1 1 1 0))


(defn ->cycle
  "Takes the base of a raw, nested op structure and turns it into a timed event Cycle."
  ([op] (->cycle op base-context))
  ([op ctx]
   (-> op
       (operate ctx)
       e/collapse-events)))


(extend-type cyclops.pattern.Operatic
  e/Cyclic
  (period [this] (e/period (->cycle this)))
  (events [this] (e/events (->cycle this)))
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


(defn apply-fit
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
  (operate [this ctx] (apply-fit this ctx))
  e/Cyclic
  (period [this] (e/period (->cycle this)))
  (events [this] (e/events (->cycle this)))
  #_e/DoYouRealize?
  #_(realize [this ctx] (e/realize (->cycle this) ctx)))


(defrecord FitOp [children]
  Operatic
  (operate [_ ctx] (operate children ctx)))




(defn basic
  "Ctrl that moves value from `:init` to `:param`"
  [param pat]
  (->> pat ->cycle (e/map-events #(e/reassoc-param % :init param))))


(defn pat-op
  "TODO: In converting merge from def-time to op-time, I broke it.
  Advantage is that ops retain a simple, declarative state
  But now we need to apply timing to ...
Merges argument pattern with value pattern to produce events."
  [op-fn arg-pat val-pat]
  (let [param    (gensym)
        args     (basic param arg-pat)
        cycl     (->cycle val-pat)
        merge-fn (fn [o e] [(get-in o [:params param]) e])
        prepared (merge/merge-cycles merge-fn args cycl :op-merge)]
    (map (fn [[arg evts]] (op-fn arg evts))
         (e/events prepared))))


(defn ->op
  ([op children]
   (op (u/smart-splat children)))
  ([op argpat children]
   (let [kids (u/smart-splat children)
         args (u/ensure-vec argpat)]
     (op args kids))))



(defrecord TimesOp1 [n children]
  Operatic
  (operate [_ ctx]
    (apply-fit (u/cycle-n n children) ctx)))


(comment (-> (->TimesOp1 3 [:a]) ->cycle e/events)
         (-> [(->TimesOp1 3 [:a]) :b] ->cycle e/events))

(comment
  (pat-op ->TimesOp1 [2 2] [:a :b]))

(defrecord TimesOp [n* children]
  Operatic
  (operate [_ ctx]
    (apply-fit
     (pat-op ->TimesOp1 n* children)
     ctx)))


(comment
  (-> (->TimesOp [2] [:a]) ->cycle e/events)
  (-> (->TimesOp [2] [:a :b]) ->cycle e/events)
  (-> (->TimesOp [2 2] [:a :b]) ->cycle e/events)
  (-> (->FitOp [:c (->TimesOp [2] [:a])]) ->cycle e/events)
  (-> (->FitOp [(->TimesOp [2] [:a :b]) :c]) ->cycle e/events)
  (-> (->FitOp [(->TimesOp [2 2] [:a :b]) :c]) ->cycle e/events))


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


(defrecord RepOp1 [n children]
  Operatic
  (operate [_ ctx]
    (splice (cycle-n n children) ctx))
  Weighty
  (weigh [_] (* n (sum-weights children))))


(comment
  (-> (->RepOp1 2 [:a :b]) ->cycle :events)
  (-> (->FitOp [(->RepOp1 2 [:a :b]) :c]) ->cycle :events))


(defrecord RepOp [n* children]
  Operatic
  (operate [_ ctx]
    (splice
     (pat-op ->RepOp1 n* children)
     ctx))
  Weighty
  (weigh [_] (sum-weights (pat-op ->RepOp1 n* children))))


(comment
  (-> (->RepOp [2] [:a]) ->cycle :events)
  (-> (->RepOp [2] [:a]) weigh)
  (-> (->RepOp [2] [:a :b]) ->cycle :events)
  (-> (->RepOp [2] [:a :b]) weigh)
  (-> (->RepOp [2 2] [:a :b]) ->cycle :events)

  (-> (->FitOp [(->RepOp [2] [:a]) :c]) ->cycle :events)
  (-> (->FitOp [(->RepOp [2] [:a :b]) :c]) ->cycle :events)
  (-> (->FitOp [(->RepOp [2 2] [:a :b]) :c]) ->cycle :events)

  (-> (->SlowOp [3] [(->RepOp [2] [:a]) :b]) ->cycle :events)
  (-> (->SlowOp [5] [(->RepOp [2] [:a :b]) :c]) ->cycle :events)
  (-> (->SlowOp [5] [(->RepOp [2 2] [:a :b]) :c]) ->cycle :events)


  )




(defrecord RevOp [children]
  Operatic
  (operate [_ ctx]
    (let [evts (operate children ctx)]
      evts
      #_(-> evts
          (map (fn [e] (update e :start #(- p %))))
          sort)))
  Weighty
  (weigh [_] (sum-weights children)))


(comment
  (-> (->RevOp [:a [:b :c]]) (operate base-context)))

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


#_(defn apply-slow
  "Stretch the children across `factor` segments by altering `period` and stretching `spacing` and `segment-length`."
  [x children ctx]
  (let [xf (partial * x)
        timed (if (every? e/event? children) children (apply-fit children ctx))]
    (map #(e/event-xf xf % #{:start :length :period}) timed)))


(defrecord SlowOp1 [x children]
  Operatic
  (operate [_ ctx]
    (apply-slow x children ctx)))


(comment
  (-> (->SlowOp1 2 [:a]) ->cycle :events)
  (-> (->SlowOp1 2 [:a :b]) ->cycle :events)
  (-> (->SlowOp1 2 [:a :b :c]) ->cycle :events)
  (-> (->FitOp [(->SlowOp1 2 [:a :b]) :c]) ->cycle :events)
  )


(defrecord SlowOp [x* children]
  Operatic
  (operate [_ ctx]
    (apply-fit
     (pat-op ->SlowOp1 x* children)
     ctx)))


(comment
  (-> (->SlowOp [2] [:sd]) ->cycle :events)
  (-> (->SlowOp [2] [:sd :bd]) ->cycle :events)
  (-> (->SlowOp [2 2] [:sd :bd]) ->cycle :events)
  (-> (->SlowOp [2 1] [:sd :bd]) ->cycle :events)

  (-> (->FitOp [:cr (->SlowOp [2] [:sd])]) ->cycle :events)
  (-> (->FitOp [:cr (->SlowOp [2] [:sd :bd])])   ->cycle :events)
  (-> (->FitOp [:cr (->SlowOp [2 2] [:sd :bd])]) ->cycle :events)
  (-> (->FitOp [:cr (->SlowOp [2 1] [:sd :bd])]) ->cycle :events)


  )


(defrecord CyclOp [children]
  Operatic
  (operate [_ ctx]
    (apply-slow (sum-weights children) children ctx)))


(comment
  (-> (->CyclOp [:sd])         ->cycle :events)
  (-> (->CyclOp [:sd :bd])     ->cycle :events)
  (-> (->CyclOp [:sd :bd :cr]) ->cycle :events)
  )


;; TODO: Can't maybe a group? only each element
(defrecord MaybeOp1 [x children]
  Operatic
  (operate [_ ctx]
    (splice (map (u/p u/maybe x) children) ctx))
  Weighty
  (weigh [_] (sum-weights children)))


(comment
  (-> (->MaybeOp1 1/2 [:a :b]) (e/realize nil))
  (-> (->MaybeOp1 1/2 [[:a :b]]) (e/realize nil))
  (-> (->MaybeOp1 1/2 [:c [:a :b]]) (e/realize nil))
  (-> [:c (->MaybeOp1 1/2 [:a :b])] (e/realize nil))
  (-> (->CyclOp [(->MaybeOp1 1/2 [:b :c]) :a]) (e/realize nil))
  (-> [:c (->MaybeOp1 1/2 [(->SpliceOp [:a :b])])] (e/realize nil))
  ,)


(defrecord MaybeOp [x* children mode]
  Operatic
  (operate [_ ctx]
    (letfn [(maybe1 [x kids]
              (->MaybeOp1 x (cond-> (map e/get-init kids)
                              (= mode :all) vector)))]
      (splice
       (pat-op maybe1 x* children)
       ctx)))
  Weighty
  (weigh [_] (sum-weights children)))


(comment
  (-> (->MaybeOp [0 1] [:a :b] :all) (e/realize nil))
  (-> (->MaybeOp [1/2 1] [:a :b] :all) (e/realize nil))
  (-> (->MaybeOp [1/2] [[:a :b]] :per) (e/realize nil))
  (-> (->MaybeOp [0 1 1/2] [:a :b [:c :d :e]] :all) (e/realize nil))
  (-> [(->MaybeOp [1/2] [(->SpliceOp [:a :b])] :per) :c] (e/realize nil))
  (-> [(->MaybeOp [1/2] [(->SpliceOp [:a :b])] :all) :c] (e/realize nil))
  ,)


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


(defrecord EuclidOp [k n r children]
  Operatic
  (operate [_ ctx]
    (let [mask            (bjork (repeat k [true]) (repeat (- n k) [nil]))
          mask            (u/rot mask (or r 0))
          children        (map #(and % children) mask)]
      (splice children ctx)))
  Weighty
  (weigh [_]
    (* n (weigh val))))


(defrecord PickOp [children]
  Operatic
  (operate [_ ctx]
    (splice [#(rand-nth children)] ctx))
  Weighty
  (weigh [_] (weigh (first children))))


(defrecord ElongateOp1 [n children]
  Operatic
  (operate [_ ctx]
    (let [n              (sum-weights children)
          segment-length (/ (:segment-length ctx) n)
          spacing        segment-length]
      (apply-timing children (assoc ctx
                                    :segment-length segment-length
                                    :spacing spacing))))
  Weighty
  (weigh [_] (* n (sum-weights children))))


(defrecord ElongateOp [n* children]
  Operatic
  (operate [_ ctx]
    (splice (pat-op ->ElongateOp1 n* children) ctx))
  Weighty
  (weigh [_] (sum-weights (pat-op ->ElongateOp1 n* children))))



(defrecord StackOp [children]
  Operatic
  (operate [_ ctx]
    (mapcat
     #(operate % ctx)
     (map vector children)))
  Weighty
  (weigh [_] (apply max (map weigh children))))


;; Controls
(defn ->ctrl
  [param value-tx pat]
  (let [cyc
        (->> pat
             ->cycle
             (e/map-events
              #(-> % (e/reassoc-param :init param (fn [v] (u/defer value-tx v))))))]
    cyc))



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
