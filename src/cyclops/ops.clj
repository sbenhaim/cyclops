(ns cyclops.ops
  (:require
   [cyclops.pattern :as p :refer [->op]]
   [cyclops.util :as u :refer [smart-splat collate]]
   [cyclops.merge :as m]
   [cyclops.events :as e]))


(defn evts [rable]
  (e/realize rable nil))


(defn fit
  "Squeezes it's children into the confining space."
  [& children]
  (->op p/->FitOp children))


(comment (-> (fit :a :b [:c :d] :e) evts))


(defn x [n-pat & children]
  (->op p/->TimesOp n-pat children))


(comment
  (->> [:a :b] (x 2) evts)
  (->> [:a (x 2 :b) :c] evts)
  (->> [:a :b] (x [2 2]) evts)
  (->> [:a :b] (x [2 2 1]) evts))


(defn spl
  "Splices events into parent context adjusting segmentation."
  [& children]
  (->op p/->SpliceOp children))


(comment
  (-> [:a :b (spl :c :d)] evts))


(defn rep [n-pat & children]
  (->op p/->RepOp n-pat children))


(comment
  (evts (rep 2 :sd))
  (evts (rep [2 2 1] :sd :bd))
  (evts
   [:a (rep [2 3] :b :c) :d]))


(defn slow [x-pat & children]
  (->op p/->SlowOp x-pat children))



(comment

  ;; Doesn't
  (evts (slow [2 2] :bd :sd))

  ;; Works
  (evts (slow 2 :bd :sd))


  (evts (slow [2 1 2] :bd :sd :cr))
  )


(defn cyc
  "Stretches children across n cycles."
  [& children]
  (->op p/->CycleOp children))

(comment
  (->> (cyc :a [:b :c]) evts))


(defn may
  [x-pat & children]
  (->op p/->MaybeOp x-pat children))

(comment
  (evts (may 0.5 :a))
  (e/realize (may [1 0.5] :a :b) nil))


(defn euc
  "Euclidian rhythm of `k` active of `n` switches, optionally rotated by `r`."
  [[k n & [r]] & children]
  (p/->EuclidOp k n r (smart-splat children)))

(comment
  (evts
   (euc [3 4] :a)))

(defn pick
  "Each loop, randomly chooses one of its children."
  [& children]
  (->op p/->PickOp children))


(defn el
  [n-pat & children]
  (->op p/->ElongateOp n-pat children))


(defn stack
  "Plays contained patterns or events simultaneously. Can be used to play chords."
  [& children]
  (->op p/->StackOp children))


;; Controls

(defn s
  "Samples and synths"
  [& pat]
  (p/->ctrl :s p/parse-sound (smart-splat pat)))


(defn n
  "Numbers and notes."
  [& pat]
  (p/->ctrl :n p/parse-num (smart-splat pat)))


(comment (-> (n (euc [3 5] :d) (pick :a :d) [:b (may 1/2 :c)]) evts))


(defn pan
  "Left 0.0, Right 1.0"
  [& pat]
  (p/->ctrl :pan (collate float) (smart-splat pat)))


(defn vowel
  ":a :e :i :o :u"
  [& pat]
  (p/->ctrl :vowel (collate name) (smart-splat pat)))


(defn room
  "Reverb room size"
  [& pat]
  (p/->ctrl :room float (smart-splat pat)))


(defn size
  "Reverb size"
  [& pat]
  (p/->ctrl :size float (smart-splat pat)))


(defn dry
  "Reverb dry"
  [& pat]
  (p/->ctrl :dry float (smart-splat pat)))


(defn legato
  "Play note for `n` segments, then cut."
  [& pat]
  (p/->ctrl :legato float (smart-splat pat)))


(defn rev-cycl ;; TODO: How to unify? Also, this doesn't work.
  "Reverse cycle"
  [& cycl]
  (e/q-cycle-xf cycl (fn [evts]
                       (->> evts
                            (map (fn [e] (update e :start #(- (e/period cycl) %))))
                            (sort e/event-sort)))))


(defn rev-pat
  "Reverse pat"
  [& pat]
  (reverse (smart-splat pat)))



(defn f| [f & cycles]
  (m/merge-cycles f (smart-splat cycles)))


(comment
  (->
   (f| (m/apply|fn-merge #(u/vector* (:prev %) (:cur %)))
       (n :a)
       (n :b))
   evts)

  (->
   (f| m/apply|left-merge
       (n :a)
       (n :b))
   evts)

  (->
   (f| m/stack-merge
       (n 1)
       (n inc))
   evts)

  (->
   (f| m/apply|stack-merge
       (n :a)
       (n #(inc (:prev %)))
       (n :b))
   evts)

  (->
   (f| m/apply|stack-merge
       (n 1)
       (n #(inc 2)))
   evts)


  (->
   (f| m/apply|stack-merge
       (n 1)
       (s :b))
   evts)


  (->
   (f| (m/apply|maths|stack-merge +)
       (s 1)
       (s #(inc (:prev %)))
       (s #(rand-int 10)))
   evts)


  ,)


(defn f> [f & cycles]
  (m/merge-cycles f (smart-splat cycles) :left))


(defn <f [f & cycles]
  (m/merge-cycles f (reverse (smart-splat cycles)) :left))


(defn s| [& cycles]
  (apply f| m/apply|stack-merge cycles))


(comment
  (->
   (s| (n :a)
       (n #(rand-nth [:b :c :d])))
   evts)

  (->
   (s| (n 1)
       (n #(inc (:prev %)))
       (n 2))
   evts))


(defn m| [f & cycles]
  (apply f| (m/apply|maths|stack-merge f) cycles))


(comment
  (-> (m| + (n 1) (n 2)) evts)
  (-> (m| + (n #(rand-int 10)) (n 2)) evts)
  (-> (m| + (n #(rand-int 10)) (n #(inc (/ (:prev %) 2)))) evts)
  (-> (m| + (n :a) (n 4)) evts)
  (-> (m| + (s :a) (s :b)) evts)
  (-> (m| + (n :a) (s :b)) evts)
  ,)


(defn m> [f & cycles]
  (apply f> (m/apply|maths|stack-merge f) cycles))


(defn <m [f & cycles]
  (apply <f (m/apply|maths|stack-merge f) cycles))


(defn +| [& cycles]
  (apply m| + cycles))


(defn +> [& cycles]
  (apply m> + cycles))


(defn <+ [& cycles]
  (apply <m + cycles))


(defn jux [tx cyc]
  (s|
   (+| cyc (pan 0))
   (+| (tx cyc) (pan 1))))


;; TODO: Pattern accepting time controls?
;; note("c2, eb3 g3 [bb3 c4]").sound("piano").slow("0.5,1,1.5")
;; Implementation could just be stack merge of three different slow pats
;; Suggests mix and match of cycle merges (stack) and patterns (slow)
;; Way to make pats and cycles more uniform, so things like `rev` and `slow`
;; Can act on both?

(defn x+
  "Stretches children across n cycles."
  [arg-pat & children]
  (apply f| (m/apply|maths|stack-merge *) arg-pat (x 1 children)))

(evts
 (f| (m/fn-merge +)
     [#(rand-int 10) 1]
     [#(rand-int 10) 2 3]))



(defn sin
  ([] (sin 0 1 1))
  ([max] (sin 0 max 1))
  ([min max] (sin min max 1))
  ([min max period]
   (fn [{:keys [event]}]
     (let [TAU  (* 2 Math/PI)
           mult (-> max (- min) (/ 2))
           base (-> (:start event) (* TAU) (/ period) Math/sin (+ 1) (* mult))
           n    (+ base min)]
       n))))


(defn rand
  ([] (rand 0 1))
  ([max] (rand 0 max))
  ([min max]
   (fn []
     (let [cap (- max min)]
       (+ (clojure.core/rand cap) min)))))


(defn irand
  ([max] (irand 0 max))
  ([min max]
   (fn []
     (let [cap (- max min)]
       (+ (rand-int cap) min)))))
