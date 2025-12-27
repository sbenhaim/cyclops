(ns cycl.merge
  (:require [cycl.event :as e]
            [cycl.util :as u]
            [cycl.val :as v]
            [cycl.cycl :as c]))


(defn left-merge
  "First value wins"
  [a _b] a)


(defrecord FnMerge [mfn a b]
  v/DoYouRealize?
  (realize [_ ctx]
    (v/realize
     (mfn (v/realize a ctx)
          (v/realize b ctx))
     ctx)))


(defn fn-merge
  "Workhorse"
  [f]
  (fn [a b]
    (->FnMerge f a b)))


(def stack-merge
  "Combine as vector, i.e. played simultaneously"
  (fn-merge (fn [a b] (u/set* a b))))


(def or-merge
  "First truthy value wins."
  (fn-merge #(or %1 %2)))


(defn apply-merge
  "If b is fn, apply to a"
  [a b]
  (v/->Realize+Apply b a))


(defn apply|fn-merge
  "If b is fn, apply to a. Otherwise apply fn `f` to realized values of a and b"
  [f]
  (fn [a b]
    (if (fn? b)
      (v/->Realize+Apply b a)
      (->FnMerge f a b))))


(def apply|left-merge
  (apply|fn-merge left-merge))


(def apply|stack-merge
  (apply|fn-merge stack-merge))


(defn apply|maths|or|stack-merge
  "Complex but arguably intuitive combination of merge behavior where:
  1. If b is fn, apply to a
  2. If both a and b (left/right) realize to numerics--or nil/false treated as 0--pass as args to provided numeric function `f` (like `+` or `*`)
  3. If one of a or b is nil/false, choose the other
  4. Stack them
"
  [f]
  (apply|fn-merge
   (fn [a b]
     (cond
       (u/num-enough? a b) (f (or a 0) (or b 0))
       (and a b) (u/set* a b)
       :else (or a b)))))


(defn apply|maths|or|left-merge
  [f]
  (apply|fn-merge
   (fn [a b]
     (cond
       (u/num-enough? a b) (f (or a 0) (or b 0))
       (and a b) a
       :else (or a b)))))



(defn merge-cycles-left
  [merge-fn cycl-a cycl-b]
  (map (fn [a]
         (let [a-start (e/start a)
               [b]     (filter #(and
                                 (<= (e/start %) a-start)
                                 (> (e/end %) a-start))
                               cycl-b)]
           (if b
             (update a :params
                     #(merge-with merge-fn (:params b) %))
             a)))
       cycl-a))


(comment
  (let [a [{:start 0 :length 1 :params {:init :a}}]
        b [{:start 0 :length 1/2 :params {:init :b1}}
           {:start 1/2 :length 1/2 :params {:init :b2}}]]
    (merge-cycles-left u/set* a b))
  (let [a [{:start 0 :length 1/2 :params {:init :a1}}
           {:start 1/2 :length 1/2 :params {:init :a2}}]
        b [{:start 1/2 :length 1/2 :params {:init :b}}]]
    (merge-cycles-left u/set* a b))
  (let [a [{:start 0 :length 1/3 :params {:init :a1}}
           {:start 1/3 :length 2/3 :params {:init :a2}}]
        b [{:start 0 :length 1/2 :params {:init :b1}}
           {:start 1/2 :length 1/2 :params {:init :b2}}]]
    (merge-cycles-left u/set* a b)))


(defn slices
  [a b]
  (let [ab     (concat a b)
        starts (map e/start ab)
        ends   (map e/end ab)]
    (->> (concat starts ends)
         sort
         distinct
         (partition 2 1))))


(defn merge-cycles-split
  [merge-fn cycl-a cycl-b]
  (let [slices (slices cycl-a cycl-b)]
    (map
     (fn [[start end]]
       (let [[a]    (c/slice-active cycl-a start end)
             [b]    (c/slice-active cycl-b start end)
             params (cond
                      (nil? b) (:params a)
                      (nil? a) (:params b)
                      :else    (merge-with merge-fn (:params a) (:params b)))]
         (e/->event params start (- end start))))
     slices)))


(comment
  (let [a [{:start 0 :length 1/2 :params {:init :a}}]
        b [{:start 1/4 :length 1/2 :params {:init :b1}}
           {:start 1/2 :length 1/2 :params {:init :b2}}]]
    (merge-cycles-split u/set* a b))
  (let [a [{:start 0 :length 1/2 :params {:init :a}}]
        b [{:start 1/2 :length 1/2 :params {:init :b}}]]
    (merge-cycles-split u/set* a b))
  (let [a [{:start 1/3 :length 1/3 :params {:init :a}}]
        b [{:start 0 :length 1/2 :params {:init :b1}}
           {:start 1/2 :length 1/2 :params {:init :b2}}]]
    (merge-cycles-split u/set* a b))
  (let [a [{:start 0 :length 1/3 :params {:init :a1}}
           {:start 1/3 :length 1/3 :params {:init :a2}}
           {:start 2/3 :length 1/3 :params {:init :a3}}]
        b [{:start 0 :length 1/2 :params {:init :b}}]]
    (merge-cycles-split u/set* a b)))
