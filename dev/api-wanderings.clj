(ns scratch
  {:clj-kondo/ignore true}
  (:require [cyclops.pattern :as p]
            [cyclops.ops :as o]
            [clojure.walk :as w]
            [cyclops.util :as u]))


;; API

;; # Ops

(fit :a :b :c) ; [x] Variadic form.

(fit [:a :b :c]) ; [x] Smart splat form

(cyc :a :b (fit :c :d)) ; [x] Nested literal form

(cyc :a :b [:c :d]) ; [x] Nested seq form. TODO: This allowed? Yes

`(:a :b [:c :d]) ; [m;ny] Insane form. TODO: This allowed?
`[:a :b (:c :d)] ; [m;ny] Probably not. But it's terse
`[:a :b (:c #{:d :e :f})] ; [m;ny]. Literals but not data structures? Is that even possible? Macro?

(-> (cyc :a :b :c) rev) ; [x] rev on OpTree

(n (cyc :a :b :c)) ; [x] ctrl on OpTree. TODO: What type returned? cycl + ctrl fn

(n :a :b :c)  ; [x] ctrl on default Ops. Fit or Splice? [same!] Only matters for nesting? TODO: This allowed? Yes.

(-> (fit :a :b :c) n rev) ; [x] Rev on ctrl. TODO: this allowed? Yes! works!

(+| (n :a :b :c) (s :d :e :f)) ; [x] Merge on ctrls

(+| (fit :a :b :c) (fit :d :e :f)) ; [x] Merge on OpTrees TODO: This allowed? Yes! Naked events (init param)


(+| (*| a b) (%| a b)) ; [x] Merge on Merges

(slow [1 2 3] :a :b :c) ; [x] OpTree

(slow [1 2 3] (n :a :b :c)) ; [x] Works
                                        ; [p] Do all ops work on Cycles? Yes. And arrays of maps!

;; What would a data object that combines OpTrees, ctrls, and merges look like?
;;
;; Assumptions:
;; 1. OpTree can have only 1 ctrl
;; 2. ctrl can have only 1 OpTree (pattern?)
;; 3. merge has * children, including OpTrees, Ctrls, and merges

;; Combining Ops and Ctrl

(comment                                ;NOPE


  #Pattern{:ctrl cfn :op op-tree}       ; [-]

                                        ; or

  {:ctrl cfn|nil :op op-tree}           ; [-]

                                        ;or

  [op-tree cfn|nil]                     ; [-]

  ;; Merge

  #SuperGroup{:with mfn :pats patterns-or-mgs} ; [-]

  (defprotocol Slice+Dice
    (period [this])
    (events [this]))

  (extend-protocol Slice+Dice
    MergeGroup
    (period [this] (gcp (:pats this)))
    (events [this] (->events this))
    Pattern
    (period [this] (p/period this))
    (events [this] (->events this)))

  ;; Applying ctrl is just populating the record field
  ;; Anything sliceable is mergeable, so include MGs


  (defn apply-timing
    [pat {:keys [period spacing segment-length start] :as context}]
    (loop [pat pat start start events []]
      (if-not
          (seq pat) events
          (let [[child & pat] pat
                weight        (weigh child) ;; How many segments to occupy
                length        (* weight segment-length)
                ctx           (assoc context
                                     :start start
                                     :segment-length length) ;; Context for children operations
                next-start    (+ start (* weight spacing))]
            (recur pat next-start
                   (conj events
                         (cond
                           (op? child)      (operate child ctx) ;; If child is an op, apply with inherited context
                           (e/event? child) (assoc child :start start :length length :period period)
                           :else            (e/->event child ;; Otherwise, add timing information to the event. Event keys can override (e.g., `length`).
                                                       start
                                                       length
                                                       period))))))))

  (defn fit [& children]
    (let [n    (count children)
          frac (/ n)]
      (mapcat
       (fn [i cs]
         (map (fn [c]
                (-> c
                    (update :length #(* (or % 1) frac))
                    (update :start #(* (or % 0) frac))
                    (update :start (partial + (* i frac)))))
              (u/gimme-vec cs)))
       (range n)
       children)))


  (defn cyc [& children]
    (let [n (count children)]
      (mapcat
       (fn [i cs]
         (map (fn [c]
                (let [p (* (get c :period 1) 1 n)]
                  (-> c
                      (assoc :period p)
                      (update :start #(+ (or % 0) (* i (/ p n)))))))
              (u/gimme-vec cs)))
       (range n)
       children)))


  (fit {} (fit {} {} {}) (fit {} {}))

  (cyc (fit {:n :a} (cyc {:n :b} {:n :c}))
       {:n :d})

  )
