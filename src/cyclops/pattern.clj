(ns cyclops.pattern
  (:require
   [cyclops.util :refer [cycle-n rot]]
   [cyclops.music :as m]))


(defn ->event
  "Expands shorthand notation to an (untimed) event map."
  [sh]
  (cond
    (coll? sh) sh
    :else {:value sh}))


(defn ->events
  "See `cyclops.pattern/->event`"
  [shs]
  (map ->event shs))


(defrecord Context
    [period         ;; Number of cycles before looping
     segment-length ;; How much segments does it occupy per cycle
     spacing        ;; How many segments between events
     start          ;; Where it sits in the pattern
     ])


(defprotocol Op
  (weight [this])
  (apply-op [this context])
  (display [this]))


(defn op? [e] (satisfies? Op e))


(defn event? [e] (not (op? e)))


(defn weigh [e]
  (if (op? e) (weight e) 1))


(def base-context (->Context 1 1 1 0))


(defn process-pattern
  [pat-root]
  (-> pat-root (apply-op base-context) flatten))


(defn display-pat
  [pat]
  (let [p (process-pattern pat)]
    p))


(defn weigh-children
  "Adds the weights of all children of an op recursively."
  [children]
  (reduce + (map weigh children)))


(defn apply-timing
  "Given a collection of events and a timing `Context`, recursively schedules
  the events honoring weights."
  [events ^Context {:keys [period spacing segment-length start] :as context}]
  (loop [raw events start start timed []]
    (if-not
        (seq raw) timed
        (let [[evt & raw] raw
              evt-weight  (weigh evt) ;; How many segments to occupy
              evt-length  (* evt-weight segment-length)
              op-ctx      (assoc context
                                 :start start
                                 :segment-length evt-length) ;; Context for children operations
              next-start  (+ start (* evt-weight spacing))]
          (recur raw next-start
                 (conj timed
                       (if (op? evt)
                         (apply-op evt op-ctx) ;; If child is an op, apply with inherited context
                         ;; Otherwise, add timing information to the event. Event keys can override (e.g., `length`).
                         (let [evt (if (map? evt) evt {:value evt})]
                           (merge {:start  start
                                   :period period
                                   :length evt-length
                                   :end    (+ start evt-length)} evt)))))))))


(defn fit-children
  "Squeeze children into inherited segment. Spacing and segment length will be the same. Period remains unchanged."
  [children {:keys [segment-length] :as context}]
  (let [n              (weigh-children children)
        spacing        (/ segment-length n)
        segment-length spacing]
    (apply-timing children (assoc context
                                  :spacing spacing
                                  :segment-length segment-length))))



;; Pattern API: Functions used in pattern definition

;; Any sequence treated like Tidal's `fastcat`, squeezing notes into the containing context.
(extend-protocol Op
  clojure.lang.Sequential
  (weight [_] 1)
  (apply-op [this context] (fit-children this context))
  (display [this] this))


(comment (process-pattern [:a :b :c]))


(defn apply-splice
  [children {:keys [segment-length] :as context}]
  (let [n              (weigh-children children)
        segment-length (/ segment-length n)]
    (apply-timing children (assoc context
                                  :spacing segment-length
                                  :segment-length segment-length))))


(defmacro defop
  {:clj-kondo/lint-as :clojure.core/defn :clj-kondo/ignore true}
  [op docs args & methods]
  (let [apply-op (first (filter #(= 'apply-op (first %)) methods))
        weight (first (filter #(= 'weight (first %)) methods))
        weight (or weight '(weight [_] 1))
        display (first (filter #(= 'display (first %)) methods))
        display (or display '(display [this] (display-pat this)))]
    (assert apply-op "`apply-op` must be defined for op.")
    `(defn ~op ~docs [~@args]
       (reify Op
         ~weight
         ~apply-op
         ~display))))


(defop spl
  "Splices events into parent context adjusting segmentation."
  [& children]
  (weight [_] (weigh-children children))
  (apply-op [_ ctx] (apply-splice children ctx)))


(defn times
  "Squeezes all of its events into the enclosing segment."
  [n & children]
  (cycle-n n children))


(defn rep [x & children]
  (apply spl (cycle-n x children)))


(defn prob
  "Plays event with probability `x` (0 to 1). Plays rest otherwise.
  If applied to group, prob is applied to *each* event, not to entire group."
  [x & children]
  (apply spl (map (fn [e] (fn [& _] (when (< (rand) x) e))) children)))



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

(euclid [5 8] :bd)

(defn pick
  "Each loop, randomly chooses one of its children."
  [& children]
  (apply spl [#(rand-nth children)]))


(defn apply-slow
  "Stretch the children across `factor` segments by altering `period` and stretching `spacing` and `segment-length`."
  [factor children {:keys [period segment-length] :as context}]
  (let [n              (weigh-children children)
        cycle-period   (* factor period)
        spacing        (/ cycle-period n)
        segment-length (/ (* segment-length factor) n)]
    (apply-timing children (assoc context
                                  :period cycle-period
                                  :spacing spacing
                                  :segment-length segment-length))))


(defop slow
  "Stretches children across n cycles."
  [n & children]
  (apply-op [_ ctx] (apply-slow n children ctx)))


(defn cyc
  "Plays one child per cycle."
  [& children]
  (apply slow (weigh-children children) children))


(defop elongate
  "Stretches note across `n` segments."
  [n & children]
  (weight [_] (* n (weigh-children children)))
  (apply-op [_ {:keys [segment-length] :as context}]
            (let [n              (weigh-children children)
                  segment-length (/ segment-length n)
                  spacing        segment-length]
              (apply-timing children (assoc context :segment-length segment-length :spacing spacing)))))


(defop stack
  "Plays contained loops simultaneously."
  [& children]
  (apply-op [_ ctx] (mapcat #(apply-op % ctx) children)))


;; Effects


(defn effect-fn
  [pat target value-tx]
  (->> pat
       process-pattern
       (map #(assoc % target (value-tx (:value %))))))


(defn rest? [v]
  (or (nil? v) (#{:- "~"} v)))


(defn parse-n
  [n]
  (cond
    (rest? n) nil
    (keyword? n) (m/note n)
    (string? n) (m/note n)
    (number? n) (float n)
    :else n))


(defn n [& pat]
  (effect-fn pat :n parse-n))


(comment (n :a [:b :c]))


(defn parse-s
  [s]
  (cond
    (rest? s) nil
    (keyword? s) (name s)
    :else s))


(defn s [& pat]
  (effect-fn pat :s parse-s))


(comment (s :bd :- :bd :sd))

(defn pan [& pat]
  (effect-fn pat :pan float))

(defn vowel [& pat]
  (effect-fn pat :vowel name))

(defn room [& pat]
  (effect-fn pat :room float))

(defn size [& pat]
  (effect-fn pat :size float))

(defn dry [& pat]
  (effect-fn pat :dry float))

