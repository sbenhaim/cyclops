(ns cycl.sub-op
  (:require [cycl.event :as e]
            [cycl.cycl :as c]
            [cycl.util :as u]))


(defn weigh
  [cycl]
  (or (-> cycl meta :weight) 1))


(defn scale-op
  "Slows a cycle by a factor of x (or speeds it up if (< x 1)."
  [x cycl]
  (c/scale x cycl))


(defn arrange
  [f cycls]
  (let [weights (map weigh cycls)
        n       (reduce + weights)
        offsets (reductions + 0 weights)]
    (-> (mapcat
         (fn [cycl weight offset]
           (f cycl weight offset n))
         cycls
         weights
         offsets)
        (c/normalize))))


(defn fit-op
  "Fits a collection of cycls into a single cycle by recursively compressing starts and lengths.
  Only impacts fractional start times, i.e., does not alter inter-cycle position or period."
  [cycls]
  (arrange
   (fn [cycl weight offset n]
     (for [evt cycl]
       (let [start        (:start evt)
             [cycle frac] (u/mixed start)
             scale        (/ n)]
         (-> evt
             (assoc :start (+ cycle
                              (* offset scale)
                              (* frac scale weight)))
             (update :length #(* % scale weight))))))
   cycls))


(comment
  (fit-op [[{:params {:init :a}, :start 0, :length 1, :period 1}]
           [{:params {:init :b}, :start 0, :length 1, :period 1}]
           [{:params {:init :c}, :start 0, :length 1, :period 1}]])
  (fit-op [[{:params {:init :a}, :start 0, :length 1, :period 1}]
           [{:params {:init :b}, :start 0, :length 1, :period 1}]
           (fit-op [[{:params {:init :c}, :start 0, :length 1, :period 1}]
                    [{:params {:init :d}, :start 0, :length 1, :period 1}]])])
  (fit-op [[{:params {:init :a}, :start 0, :length 1, :period 1}]
           [{:params {:init :b}, :start 0, :length 1, :period 1}]
           (with-meta
             (fit-op [[{:params {:init :c}, :start 0, :length 1, :period 1}]
                      [{:params {:init :d}, :start 0, :length 1, :period 1}]])
             {:weight 2})]))


(defn cycl-op
  "Cycls through a collection of cycls one per cycle."
  [cycls]
  (arrange
   (fn [cycl weight offset n]
     (for [evt cycl]
       (let [start        (:start evt)
             [cycle frac] (u/mixed start)]
         (-> evt
             (assoc :start (+ (* cycle n)
                              offset
                              (* frac weight)))
             (update :length #(* % weight))
             (update :period #(* % n))))))
   cycls))


(comment
  (cycl-op [[{:params {:init :a}, :start 0, :length 1, :period 1}]
            [{:params {:init :b}, :start 0, :length 1, :period 1}]
            [{:params {:init :c}, :start 0, :length 1, :period 1}]])
  (cycl-op [[{:params {:init :a}, :start 0, :length 1, :period 1}]
            [{:params {:init :b}, :start 0, :length 1, :period 1}]
            (cycl-op [[{:params {:init :c}, :start 0, :length 1, :period 1}]
                      [{:params {:init :d}, :start 0, :length 1, :period 1}]])])
  (cycl-op [[{:params {:init :a}, :start 0, :length 1, :period 1}]
            [{:params {:init :b}, :start 0, :length 1, :period 1}]
            (with-meta
              (cycl-op [[{:params {:init :c}, :start 0, :length 1, :period 1}]
                        [{:params {:init :d}, :start 0, :length 1, :period 1}]])
              {:weight 2})]))


(comment
  (fit-op [[{:params {:init :a}, :start 0, :length 1, :period 1}]
           [{:params {:init :b}, :start 0, :length 1, :period 1}]
           (cycl-op [[{:params {:init :c}, :start 0, :length 1, :period 1}]
                     [{:params {:init :d}, :start 0, :length 1, :period 1}]])])
  (cycl-op [[{:params {:init :a}, :start 0, :length 1, :period 1}]
            [{:params {:init :b}, :start 0, :length 1, :period 1}]
            (fit-op [[{:params {:init :c}, :start 0, :length 1, :period 1}]
                     [{:params {:init :d}, :start 0, :length 1, :period 1}]])]))

;; Ops

(defn times-op
  "Repeats a cycl n times, speeding it up to fit into its original space."
  [n cycl]
  (let [[start length period] (c/shape cycl)]
    (-> (map #(assoc % :period length) cycl)
        (->> (c/loop-cycl n))
        (c/translate start length period))))


(comment
  (times-op 2 (fit :a))
  (fit :a (times-op 2 (fit :b)))
  (times-op 2 [{:start 1/4 :length 1/4 :period 1}])
  (times-op 4 [{:start 1/2 :length 1/2 :period 1}]))

(comment
  (times-op 2 [{:start 0 :length 1 :period 2}])
  (times-op 1 [{:start 0 :length 1/2 :period 2}
                {:start 1/2 :length 1/2 :period 2}])
  (times-op 1 [{:params {:init :a}, :start 0, :length 1/2, :period 2}
               {:params {:init :b}, :start 1/2, :length 1/2, :period 2}]))


(defn elongate-op
  "Increases the relative number of slots a cycl consumes in its surrounding pattern
  versus the default of even weighting where each item is evenly spaced."
  [x cycl]
  (with-meta cycl {:weight x}))


(comment
  (fit (elongate-op 2 (fit :a)) :b)
  (fit (elongate-op 2 (fit :a :b)) :c)
  (cyc (elongate-op 2 (fit :a)) :c))


(defn repeat-op
  "Repeats a cycl n times without speeding up, such that it takes more space
  in its surrounding pattern."
  [n cycl]
  (elongate-op n (times-op n cycl)))


(comment
  (fit (repeat-op 2 (fit :a)) :b)
  (fit (times-op 2 (fit :a :b)) :c)
  (fit (repeat-op 2 (fit :a :b)) :c))


(defn by-iter
  [cycls]
  (group-by (fn [c] (-> c first e/iter)) cycls))


(defn re-weight
  [cycls]
  (mapcat
   (fn [[iter cycls]]
     (let [lengths (map c/length cycls)
           weights (map weigh cycls)
           weighted-lengths (u/weighted lengths weights)
           starts (reductions + 0 weighted-lengths)]
       (mapcat
        (fn [c s l]
          (c/translate c (+ s iter) l (c/period c)))
        cycls
        starts
        weighted-lengths)))
   (by-iter cycls)))


(defn op-merge
  "Given an op, a cycl of arguments and an event cycl, merges the two cycls
  and performs the op based on the overlap of the argument and values.

  op1: A op fn that takes a single value as its first arg and a cycl as its second
  arg-cycl: A cycl representing arguments to `op1`
  evt-cycl: The events to which to apply the op"
  [op1 arg-cycl evt-cycl]
  (let [[arg-cycl evt-cycl] (c/normalize-periods [arg-cycl evt-cycl])
        cycls
        (map
         (fn [arg-evt]
           (let [arg     (e/get-init arg-evt)
                 overlap (c/slice evt-cycl (e/start arg-evt) (e/length arg-evt) :active-during)]
             (op1 arg overlap)))
         arg-cycl)]
    (re-weight cycls)))


(comment
  (op-merge times-op (fit 1 2) (fit :a :b))
  (op-merge times-op (cyc 1 2) (fit :a :b))

  (op-merge times-op (fit (el 2 1) 2) (fit :a :b :c))
  (op-merge times-op (fit (el 2 2) 1) (fit :a :b :c))

  (op-merge elongate-op (fit 2 1) (fit :a :b))
  (op-merge elongate-op (fit 1 (cyc 1 2 3)) (fit :a :b))


  (op-merge times-op (fit 2 (el 2 1)) (fit :a (el 2 :b)))
  (op-merge times-op (cyc (fit 2 1) (fit 1 2)) (fit :a :b))

  (op-merge elongate-op (cyc (fit 2 1) (fit 1 3)) (fit :a :b))

  (op-merge elongate-op (fit 1 (cyc 1 2 3)) (fit :a :b))

  (op-merge times-op (cyc 1 2) (cyc :a :b)))



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
