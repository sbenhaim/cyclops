(ns cycl.event
  (:require
   [cycl.util :as u :refer [p2]]
   [cycl.val :refer [realize]]
   [cycl.val :as v]))


(defn realize-event
  [e ctx]
  (let [ctx (merge ctx e)]
    (update e :params #(reduce-kv
                        (fn [m k v]
                          (assoc m k (realize v (assoc ctx :param k))))
                        {} %))))



(defn event-compare
  [this that]
  (let [f (juxt :start :length)]
    (compare (f this) (f that))))




#_(defrecord Event [params start length]
  Comparable
  (compareTo [this that]
    (event-compare this that))
  DoYouRealize?
  (realize [this ctx]
    (realize-event this ctx)))


(defn ->event
  ([init] (->event init 0 1))
  ([init start length]
   #_(->Event init start length)
   (let [params (if (and (map? init) (not (record? init)))
                  init
                  {:init init})]
     {:params params :start start :length length})))


(defn event? [evt?]
  ;; TODO:
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


(defn start [e]
  (:start e))


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


(defn overlaps?
  [a b]
  (or (and (<= (start b) (start a)) (< (start a) (end b)))
      (and (<= (start a) (start b)) (< (start b) (end a)))))


(comment
  (overlaps?
   (->event nil 1/4 1)
   (->event nil 1/2 1/4)))
