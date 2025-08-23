(ns cyclops.ops
  (:require
   [cyclops.control :as c]
   [cyclops.pattern :as p]))


(defn x
  "Repeats pattern `n` times, squeezing into single segment."
  [n children]
  (p/->TimesOp n children))


(defn x*
  "Repeats pattern `n` times, squeezing into single segment."
  [n & children]
  (x n children))


;; Splicing Ops

(defn spl
  "Splices events into parent context adjusting segmentation."
  [children]
  (p/->SpliceOp children))


(defn spl*
  "Splices events into parent context adjusting segmentation."
  [& children]
  (spl children))


(defn rep
  "Repeats `x` times without speading up adjusting segmentation."
  [x children]
  (p/->RepeatOp x children))


(defn rep*
  "Repeats `x` times without speading up adjusting segmentation."
  [x & children]
  (rep x children))


;; Period Ops


(defn slow
  "Stretches children across n cycles."
  [x children]
  (p/->SlowOp x children))


(defn slow*
  "Stretches children across n cycles."
  [x & children]
  (slow x children))


(defn cyc
  "Plays one child per cycle."
  [children]
  (p/->CycleOp children))


(defn cyc*
  "Plays one child per cycle."
  [& children]
  (cyc children))


(defn may
  "Plays event with probability `x` (0 to 1). Plays rest otherwise.
  If applied to group, prob is applied to *each* event, not to entire group."
  [x children]
  (p/->MaybeOp x children))


(defn may*
  "Plays event with probability `x` (0 to 1). Plays rest otherwise.
  If & applied to group, prob is applied to *each* event, not to entire group."
  [x children]
  (p/->MaybeOp x children))


(defn euc
  "Euclidian rhythm of `k` active of `n` switches, optionally rotated by `r`."
  [[k n & [r]] val]
  (p/->EuclidianOp k n r val))


(defn pick
  "Each loop, randomly chooses one of its children."
  [children]
  (p/->PickOp children))


(defn pick*
  "Each loop, randomly chooses one of its children."
  [& children]
  (pick children))


(defn el
  "Stretches note across `n` segments."
  [n children]
  (p/->ElongateOp n children))


(defn el*
  "Stretches note across `n` segments."
  [n & children]
  (el n children))


(defn stack
  "Plays contained patterns or events simultaneously. Can be used to play chords."
  [children]
  (p/->StackOp children))


(defn stack*
  "Plays contained patterns or events simultaneously. Can be used to play chords."
  [& children]
  (stack children))


;; Controls

(defn s
  "Samples and synths"
  [pat]
  (p/->control :s p/parse-sound pat))


(defn s*
  "Samples and synths"
  [& pat]
  (s pat))


(defn n
  "Numbers and notes."
  [pat]
  (p/->control :n p/parse-num pat))


(defn n*
  "Numbers and notes."
  [& pat]
  (n pat))



(defn pan
  "Left 0.0, Right 1.0"
  [pat]
  (p/->control :pan float pat))


(defn pan*
  "Left 0.0, Right 1.0"
  [& pat]
  (pan pat))


(defn vowel
  ":a :e :i :o :u"
  [pat]
  (p/->control :vowel name pat))


(defn vowel*
  ":a :e :i :o :u"
  [& pat]
  (vowel pat))


(defn room
  "Reverb room size"
  [pat]
  (p/->control :room float pat))


(defn room*
  "Reverb room size"
  [& pat]
  (room pat))


(defn size
  "Reverb size"
  [pat]
  (p/->control :size float pat))


(defn size*
  "Reverb size"
  [& pat]
  (size pat))


(defn dry
  "Reverb dry"
  [pat]
  (p/->control :dry float pat))


(defn dry*
  "Reverb dry"
  [& pat]
  (dry pat))


(defn legato
  "Play note for `n` segments, then cut."
  [pat]
  (p/->control :legato float pat))


(defn legato*
  "Play note for `n` segments, then cut."
  [& pat]
  (legato pat))
