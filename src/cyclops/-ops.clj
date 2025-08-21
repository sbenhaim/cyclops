(ns cyclops.ops
  (:require
   ;; [cyclops.pattern :refer [Op weigh-children apply-timing display-pat apply-op process-pattern] :as pat]
   [cyclops.pattern :as p]
   [cyclops.music :as m]
   [cyclops.util :refer [cycle-n rot]]
   [cyclops.egen :as e]))


(defn fit-children
  "Squeeze children into inherited segment. Spacing and segment length will be the same. Period remains unchanged."
  [children {:keys [segment-length] :as context}]
  (let [n              (p/weigh-children children)
        spacing        (/ segment-length n)
        segment-length spacing]
    (p/vals->events children (assoc context
                                    :spacing spacing
                                    :segment-length segment-length))))


;; Any sequence treated like Tidal's `fastcat`, squeezing notes into the containing context.
(extend-type clojure.lang.Sequential
  p/Op
  (weight [_] 1)
  (apply-op [this context] (fit-children this context))
  (display [this] this))


(defn apply-splice
  [children {:keys [segment-length] :as context}]
  (let [n              (p/weigh-children children)
        segment-length (/ segment-length n)]
    (p/vals->events children (assoc context
                                    :spacing segment-length
                                    :segment-length segment-length))))


(defmacro defop
  {:clj-kondo/lint-as :clojure.core/defn :clj-kondo/ignore true}
  [op docs args & methods]
  (let [apply-op (first (filter #(= 'apply-op (first %)) methods))
        weight (first (filter #(= 'weight (first %)) methods))
        weight (or weight '(weight [_] 1))
        display (first (filter #(= 'display (first %)) methods))
        display (or display '(display [this] (p/display-pat this)))]
    (assert apply-op "`apply-op` must be defined for op.")
    `(do
       (defn ~op ~docs [~@args]
         (reify p/Op
           ~weight
           ~apply-op
           ~display))
       (defn ~(symbol (str op "*")) ~docs [~@(butlast args) & ~(last args)]
         (~op ~@args)))))



(defop spl
  "Splices events into parent context adjusting segmentation."
  [children]
  (weight [_] (p/weigh-children children))
  (apply-op [_ ctx] (apply-splice children ctx)))


(defn x
  "Squeezes all of its events into the enclosing segment."
  [n & children]
  (cycle-n n children))


(defn rep [x children]
  (spl (cycle-n x children)))

(defn rep* [x & children] (rep x children))


(defn prob
  "Plays event with probability `x` (0 to 1). Plays rest otherwise.
  If applied to group, prob is applied to *each* event, not to entire group."
  [x children]
  (spl (map (fn [e] (fn [& _] (when (< (rand) x) e))) children)))


(defn prob* [x & children] (prob x children))


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


(defn euclid
  [[k n & [r]] evt]
  (assert (>= n k) "Second arg must be greater than first.")
  (let [mask (bjork (repeat k [true]) (repeat (- n k) [nil]))
        mask (rot mask (or r 0))]
    (map #(and % evt) mask)))


(defn pick
  "Each loop, randomly chooses one of its children."
  [children]
  (apply spl [#(rand-nth children)]))


(defn pick* [& children] (pick children))


(defn apply-slow
  "Stretch the children across `factor` segments by altering `period` and stretching `spacing` and `segment-length`."
  [factor children {:keys [period segment-length] :as context}]
  (let [n              (p/weigh-children children)
        cycle-period   (* factor period)
        spacing        (/ cycle-period n)
        segment-length (/ (* segment-length factor) n)]
    (p/vals->events children (assoc context
                                  :period cycle-period
                                  :spacing spacing
                                  :segment-length segment-length))))


(defop slow
  "Stretches children across n cycles."
  [n children]
  (apply-op [_ ctx] (apply-slow n children ctx)))


(defn cyc
  "Plays one child per cycle."
  [children]
  (slow (p/weigh-children children) children))


(defn cyc* [& children] (cyc children))


(defop elongate
  "Stretches note across `n` segments."
  [n children]
  (weight [_] (* n (p/weigh-children children)))
  (apply-op [_ {:keys [segment-length] :as context}]
            (let [n              (p/weigh-children children)
                  segment-length (/ segment-length n)
                  spacing        segment-length]
              (p/vals->events children (assoc context :segment-length segment-length :spacing spacing)))))


(defop stack
  "Plays contained loops simultaneously."
  [children]
  (apply-op [_ ctx] (mapcat #(p/apply-op % ctx) (map vector children))))

;; TODO: Can ops with no children args used std-in?


(comment
  (-> (cyc* :a :b :c)
      (p/pat->egen)
      (e/slice (e/->TimeContext 0 0 0 0 1) :begin)))

(comment
  ;; Controls
  ;;
  ;;
  (defn control-fn
    [evt-gen target value-tx]
    (let [evt-loop (p/process-pattern pat)
          evts     (map #(assoc % target (value-tx (:value %))) (.events evt-loop))]
      (assoc evt-loop :events evts)))


  (defmacro defeffect
    {:clj-kondo/lint-as :clojure.core/defn :clj-kondo/ignore true}
    ([effect doc value-tx]
     `(defeffect ~effect ~doc ~(keyword effect) ~value-tx))
    ([effect doc param value-tx]
     `(do
        (defn ~effect ~doc [pat#]
          (effect-fn pat# ~param ~value-tx))
        (defn ~(symbol (str effect "*")) ~doc [& pat#]
          (~effect pat#)))))


  (defn rest? [v]
    (or (nil? v) (#{:- "~"} v)))


  (defn parse-n
    [n]
    (cond
      (rest? n)    nil
      (keyword? n) (m/note n)
      (string? n)  (m/note n)
      (number? n)  (float n)
      :else        n))


  (defeffect n
    "Specifies midi note or sample number. Passed directly to Dirt's `n` param.
Should be a number or note name (`:c` `:e#5` `:db`)."
    parse-n)


  (defn n [& pat]
    (e/->Control (p/pat->egen pat) :n parse-n))



  (comment
    (let [gen  (p/pat->egen [(e/->SineValue 0 100 1) (e/->SineValue 0 100 1) (e/->SineValue 0 100 1) (e/->SineValue 0 100 1)])
          gen  (p/pat->egen [(e/->IRandValue 0 100) (e/->IRandValue 0 100) (e/->IRandValue 0 100) (e/->IRandValue 0 100)])
          ctrl (e/->Control gen :n parse-n)
          ctx  (e/->TimeContext 0 0 0 0 1)]
      (-> ctrl
          (e/slice ctx :active)))

    (e/slice (n :a :b :c) (e/->TimeContext 0 0 0 0 1) :active))


  (defn parse-s
    [s]
    (cond
      (rest? s)    nil
      (keyword? s) (name s)
      :else        s))


  (defeffect s
    "Specifies sample or synth. Passed directly to Dirt's `s` param. Should be a symbol or string representing a valid sample or synth."
    parse-s)


  (defeffect pan "Left 0.0, Right 1.0" float)

  (defeffect vowel ":a :e :i :o :u" name)

  (defeffect room "Reverb room size" float)

  (defeffect size "Reverb size" float)

  (defeffect dry "Reverb dry" float)

  (defeffect legato "Play note for `n` segments, then cut." float))


(comment
  (let [seg 9
        tau (* 2 Math/PI)]
    (for [i (range 0 tau (/ tau seg))]
      {:n (-> i Math/sin (+ 1) (/ 2))})))
