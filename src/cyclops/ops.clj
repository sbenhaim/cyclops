(ns cyclops.ops
  (:require
   [cyclops.pattern :as p]
   [cyclops.util :refer [smart-splat p]]
   [cyclops.merge :as m]
   [cyclops.events :as e]))


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



(defn pan
  "Left 0.0, Right 1.0"
  [& pat]
  (p/->control :pan float (smart-splat pat)))


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


(defn f| [f & cycles]
  (m/->mg f (smart-splat cycles) :both))


(defn f> [f & cycles]
  (m/->mg f (smart-splat cycles) :left))


(defn <f [f & cycles]
  (m/->mg f (reverse (smart-splat cycles)) :left))


(defn s| [& cycles]
  (apply f| m/stack-merge cycles))


(defn c| [f & cycles]
  (apply f| (m/calc-or-stack-merge f) cycles))

(defn c> [f & cycles]
  (apply f> (m/calc-or-stack-merge f) cycles))


(defn <c [f & cycles]
  (apply <f (m/calc-or-stack-merge f) cycles))


(defn +| [& cycles]
  (apply c| + cycles))


(defn +> [& cycles]
  (apply c> + cycles))


(defn <+ [& cycles]
  (apply <c + cycles))


(defn evts [cyc]
  (e/events cyc true))


(defn offset [amt cyc]
  (e/update-events cyc (partial e/offset-slice amt)))


(comment (evts (offset 1/3 (p/process-pattern (cyc :a :b :c))))) ;; TODO: Can ops implement cyclic?


(defn rev [cyc]
  (let [evts (e/events cyc false)
        timing (map #(select-keys % e/timing-keys) evts)
        the-rest (map #(apply dissoc % e/timing-keys) evts)]
    (e/set-events cyc (map merge timing (reverse the-rest)))))


(defn jux [tx cyc]
  (s|
   (+| cyc (pan 0))
   (+| (tx cyc) (pan 1))))


;; TODO: Pattern accepting time controls?
;; note("c2, eb3 g3 [bb3 c4]").sound("piano").slow("0.5,1,1.5")
