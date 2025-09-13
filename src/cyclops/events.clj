(ns cyclops.events
  (:require
   [clojure.math.numeric-tower :refer [lcm]]
   [cyclops.util :as u :refer [arity p2]]))


(defprotocol DoYouRealize?
  (realize [this ctx]))


(extend-protocol DoYouRealize?

  java.lang.Object
  (realize [this _ctx] this)

  clojure.lang.IFn
  (realize [this ctx]
    (case (arity this)
      0 (this)
      1 (this ctx)
      this)) ; Or (defer this ctx)?

  clojure.lang.ISeq
  (realize [this ctx] (map (p2 realize ctx) this)))


#_(defn realize-val
    [v ctx]
    (let [param-xfs (get-in ctx [:cycle :param-xfs (:param ctx)] []) ;; TODO: Way to do this at cycle level more cleanly?
          realized  (if (fn? v)
                      (case (arity v)
                        0 (v)
                        1 (v ctx))
                      v)]
      (u/reduce-apply realized param-xfs)))

;; NOTE: Param xfs have to happen after val realization but before merge apply|fn.
;; Should they happen in merge? Is this already ideal?


(defrecord Event [params start length period]
  Comparable
  (compareTo [this that]
    (compare [(:start this) (:length this)] [(:start that) (:length that)]))
  DoYouRealize?
  (realize [this ctx]
    (let [base     (select-keys this #{:start :length})
          ctx      (assoc ctx :event this)
          realized (into {} (for [[k v] params] [k (realize v (assoc ctx :param k))]))]
      (merge base realized))))


(defn event? [event?]
  (instance? Event event?))


(defn defer+realize
  [f & args]
  (fn [ctx]
    (apply f (map (p2 realize ctx) args))))


(defn ->event [init start length period]
  (->Event {:init init} start length period))


(defn update-param
  [e k f & vs]
  (apply update-in e [:params k] f vs))


(defn reassoc-param
  ([e from to] (reassoc-param e from to identity))
  ([e from to f]
   (-> e
       (assoc-in [:params to] (f (get-in e [:params from])))
       (update :params #(dissoc % from)))))


(defn dissoc-param
  [e param]
  (update e :params #(dissoc % param)))


(defn end [^Event e]
  (+ (:start e) (:length e)))


(defprotocol Cyclic
  (period [this])
  (events [this]))


(extend-type cyclops.events.Cyclic
  DoYouRealize?
  (realize [this ctx]
    (realize (events this) ctx)))


(defrecord Cycle [period events cycle-xfs param-xfs]
  Cyclic
  (period [_] period)
  (events [_] events)
  DoYouRealize?
  (realize [this ctx]
    (let [ctx (assoc ctx :cycle this)
          evts (map #(realize % ctx) events)]
      (u/reduce-apply evts cycle-xfs))))


(defn cycle-xfs [^Cycle cycl] (:cycle-xfs cycl))


;; TODO: Eliminate?
(defn param-xfs [^Cycle cycl] (:param-xfs cycl))


(defn ->cycle [period evts]
  (->Cycle period evts [] {}))


(defn q-cycle-xf
  [cycl xf]
  (update cycl :cycle-xfs #(conj % xf)))


(defn q-event-xf
  [cycl exf]
  (update cycl :cycle-xfs (fn [xfs] (conj xfs #(map exf (events %))))))


(defn q-param-xf
  [cycl param xf]
  (update-in cycl [:param-xfs param] #(conj (or % []) xf)))


(defn map-events
  [f cycl]
  (update cycl :events #(map f %)))


(defn map-params
  [param f cycl]
  (map-events #(update-in % [:param param] f) cycl))


(defn cycle-events
  "Like clojure.core/cycle but, moves events cyclically forward in time as it cycles"
  ([cycl] (cycle-events nil cycl))
  ([n cycl]
   (let [period (period cycl)
         evts (events cycl)
         cycle  (->> evts
                     repeat
                     (mapcat
                      (fn [i cycl]
                        (map (fn [e] (update e :start #(+ % (* i period)))) cycl))
                      (range)))]
     (if n
       (take (* n (count evts)) cycle)
       cycle))))


(defn slice [cycl from length {:keys [mode] :or {mode :starts-during}}]
  (assert (#{:starts-during :ends-during :active-during} mode))
  (let [p             (period cycl)
        evts          (events cycl)
        to            (+ from length)
        to            (if (<= to from) (+ to p) to)
        loop          (if (> to p) (cycle-events evts) evts)
        [drop? take?] (case mode
                        :starts-during [#(> from (:start %)) #(> to (:start %))]
                        :ends-during   [#(> from (end %)) #(> to (end %))]
                        :active-during [#(>= from (end %)) #(> to (:start %))])
        slc           (into []
                            (comp
                             (drop-while drop?)
                             (take-while take?))
                            loop)]
    slc))



(defn offset [amount slc]
  (map #(update % :start (partial + amount)) slc))


(defn lcp [& cycles]
  (reduce (fn [m c] (lcm m (period c))) (period (first cycles)) (rest cycles)))


(defn normalize-periods
  [a b]
  (let [p (lcp a b)
        a (cycle-events (/ p (period a)) a)
        b (cycle-events (/ p (period b)) b)]
    [(->cycle p a) (->cycle p b)]))


(defn sync-periods
  [a b]
  (let [[a b] (normalize-periods a b)]
    (->cycle (period a) (concat (events a) (events b)))))


(defn collapse-events
  "Takes a potentially nested collection of events of differing periods
  and creates a flat list of events of a single period.
  Returns a tuple of `[period evts]`.

  Note: Does not currently update :period values in the events themselves."
  [evts]
  (if-not (seq evts)
    []
    (->> evts
         flatten
         (group-by :period)
         (map #(apply ->cycle %))
         (reduce sync-periods))))
