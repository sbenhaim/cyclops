(ns cycl.event
  (:require
   [cycl.util :as u :refer [arity p2]]))



(defprotocol DoYouRealize?
  (realize [this ctx]))


(extend-protocol DoYouRealize?

  nil
  (realize [_ _] nil)

  java.lang.Object
  (realize [this _ctx] this)

  clojure.lang.IFn
  (realize [this ctx]
    (if-not (= (:param ctx) :fn)
      (case (arity this)
        0 (realize (this) ctx)
        1 (realize (this nil) ctx)
        2 (realize (this nil ctx) ctx)
        this)
      this)) ; Or (defer this ctx)?

  clojure.lang.ISeq
  (realize [this ctx] (map (p2 realize ctx) this)))


(defn event-compare
  [this that]
  (let [f (juxt :start :length)]
      (compare (f this) (f that))))



(defrecord Event [params start length period]
  Comparable
  (compareTo [this that]
    (event-compare this that))
  DoYouRealize?
  (realize [this ctx]
    (let [realized (into {} (for [[k v] params] [k (realize v (assoc ctx :param k :event this))]))]
      (assoc this :params realized))))


(defn ->event
  ([init] (->event init 0 1 1))
  ([init start length period]
   (let [init (if (map? init) init {:init init})]
     (->Event init start length period))))


(comment
  (realize (->event #(rand) 0 1/2 0 1) nil)
  (event?
   (realize (->Event {:init #(rand) :else 5 :ctx identity} 0 1/2 0 1) nil)))


(defn event? [evt?]
  (number? (:start evt?)))


(comment
  (event? (->Event {:init :hi} 0 1 0 1))
  (event? (->event :hi 0 1 0 1))
  (event? {:period 1 :start 1 :length 1})
  (event? {:period 1 :start 1}))


(defn defer+realize
  [f & args]
  (fn [ctx]
    (apply f (map (p2 realize ctx) args))))



(defn get-param
  [e param]
  (get-in e [:params param]))


(defn get-init
  [e]
  (get-param e :init))


(defn assoc-param
  [e k v]
  (assoc-in e [:params k] v))


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


(defn start [ e]
  (+ (:start e)))


(defn iter [e]
  (-> e start long))


(defn offset [e]
  (- (start e) (iter e)))


(defn position
  [e]
  [(iter e) (offset e)])


(defn end [ e]
  (+ (:start e) (:length e)))


(defn period [e]
  (:period e))


(defn length [e]
  (:length e))

(defn event-xf
  ([e f] (event-xf e f #{:start :length}))
  ([e f affected]
   (reduce (fn [e k] (update e k f)) e affected)))


(defn map-params
  [param f evts]
  (map #(update-in % [:param param] f) evts))
