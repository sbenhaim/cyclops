(ns cyclops.ops
  (:require
   [cyclops.pattern :as p]
   [cyclops.util :as u :refer [smart-splat collate]]
   [cyclops.merge :as m]
   [cyclops.events :as e]))


(defn evts [cyc]
  (e/realize cyc))


(defn x
  "Repeats pattern `n` times, squeezing into single segment."
  [n & children]
  (p/->TimesOp n (smart-splat children)))


;; Splicing Ops

(defn spl
  "Splices events into parent context adjusting segmentation."
  [& children]
  (p/->SpliceOp (smart-splat children)))


(defn rep
  "Repeats `x` times without speading up adjusting segmentation."
  [x & children]
  (p/->RepeatOp x (smart-splat children)))


;; Period Ops


(defn slow
  "Stretches children across n cycles."
  [x & children]
  (p/->SlowOp x (smart-splat children)))


(defn cyc
  "Plays one child per cycle."
  [& children]
  (p/->CycleOp (smart-splat children)))


(defn may
  "Plays event with probability `x` (0 to 1). Plays rest otherwise.
  If applied to group, prob is applied to *each* event, not to entire group."
  [x & children]
  (p/->MaybeOp x (smart-splat children)))


(defn euc
  "Euclidian rhythm of `k` active of `n` switches, optionally rotated by `r`."
  [[k n & [r]] & val]
  (p/->EuclidianOp k n r (smart-splat val)))


(defn pick
  "Each loop, randomly chooses one of its children."
  [& children]
  (p/->PickOp (smart-splat children)))


(defn el
  "Stretches note across `n` segments."
  [n & children]
  (p/->ElongateOp n (smart-splat children)))


(defn stack
  "Plays contained patterns or events simultaneously. Can be used to play chords."
  [& children]
  (p/->StackOp (smart-splat children)))


;; Controls

(defn s
  "Samples and synths"
  [& pat]
  (p/->control :s p/parse-sound (smart-splat pat)))


(defn n
  "Numbers and notes."
  [& pat]
  (p/->control :n p/parse-num (smart-splat pat)))


(comment (-> (n :a) evts))


(defn pan
  "Left 0.0, Right 1.0"
  [& pat]
  (p/->control :pan (collate float) (smart-splat pat)))


(defn vowel
  ":a :e :i :o :u"
  [& pat]
  (p/->control :vowel name (smart-splat pat)))


(defn room
  "Reverb room size"
  [& pat]
  (p/->control :room float (smart-splat pat)))


(defn size
  "Reverb size"
  [& pat]
  (p/->control :size float (smart-splat pat)))


(defn dry
  "Reverb dry"
  [& pat]
  (p/->control :dry float (smart-splat pat)))


(defn legato
  "Play note for `n` segments, then cut."
  [& pat]
  (p/->control :legato float (smart-splat pat)))


(defn rev ;; TODO: Currently unexpectedly recursive
  "Reverse pat"
  [& pat]
  (let [cycl (p/process-pattern (smart-splat pat))]
    (e/q-cycle-xf cycl (fn [evts]
                         (->> evts
                              (map (fn [e] (update e :start #(- (e/period cycl) %))))
                              (sort e/event-sort))))))


(comment (evts (rev :a [:b :c])))



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
