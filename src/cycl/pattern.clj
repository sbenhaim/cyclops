(ns cycl.pattern
  (:require [cycl.event :as e]
            [cycl.util :as u]
            [cycl.cycl :as c]
            [cycl.merge :as m]
            [cycl.music :as music]))


(defprotocol Pattern
  (gen [this start length])
  (period [this])
  (weight [this]))


(defn weigh
  [pats]
  (reduce + (map weight pats)))


(defn iter-start
  [cycle-no period]
  (* period (quot cycle-no period)))


(defn spin
  [pat]
  (let [p (period pat)]
    (gen pat 0 p)))


(defn spin*
  [pat]
  (-> pat spin (c/realize-cycl {})))


(defn lcp
  [pats]
  (if (= 1 (count pats))
    (period (first pats))
    (apply u/lcm (map period pats))))


(defrecord Pure [value]
  Pattern
  (gen [_ start length]
    (let [start (long (Math/ceil start))]
      (-> (map #(e/->event value % 1) (iterate inc start))
          (c/slice-starts start length))))
  (period [_] 1)
  (weight [_] 1))


(defrecord Lit [events p w]
  Pattern
  (gen [this start length]
    (let [i-start (iter-start start (period this))]
      (->
       (mapcat
        (fn [cycl iter]
          (for [evt cycl]
            (-> evt (update :start #(+ % iter)))))
        (repeat events)
        (iterate #(+ % p) i-start))
       (c/slice-starts start length))))
  (period [_] p)
  (weight [_] w))


(defn arrange
  ;; TODO: Calculating inf seq and then take-while always creates extra event
  [tx-evt pats]
  (let [weights  (map weight pats)
        n        (reduce + weights)
        weights* (cycle weights)
        offsets* (reductions + 0 weights*)]
    (-> (mapcat
         (fn [pat weight offset]
           (let [sub-iter-no (quot offset n)
                 sub-pat     (gen pat sub-iter-no 1)]
             (for [evt sub-pat]
               (tx-evt evt offset weight n))))
         (cycle pats)
         weights*
         offsets*))))


(defrecord Fit [pats]
  Pattern
  (gen [this start length]
    (let [i-start (iter-start (long start) (period this))]
      (->
       (arrange
        (fn [evt offset weight n]
          (let [[_ frac] (u/mixed (e/start evt))
                scale    (/ n)]
            (-> evt
                (assoc :start (+ i-start (* offset scale) (* frac scale weight)))
                (update :length #(* % scale weight)))))
        pats)
       (c/slice-starts start length))))
  (period [_] (lcp pats))
  (weight [_] 1))


(defrecord Speed [x pat]
  Pattern
  (gen [_ start length]
    (-> (gen pat (* start x) (* length x))
        (c/scale (/ x))))
  (period [_] (/ (period pat) x))
  (weight [_] (weight pat)))


(defn ->pat [x]
  (cond
    (satisfies? Pattern x) x
    (sequential? x)        (if (c/cycl? x)
                             (->Lit x 1 1)
                             (->Fit (map ->pat x)))
    :else                  (->Pure x)))



(defrecord Cyc [pats]
  Pattern
  (gen [this start length]
    (let [i-start (iter-start (long start) (period this))]
      (->
       (arrange
        (fn [evt offset weight n]
          (let [[_ frac] (u/mixed (e/start evt))]
            (-> evt
                (assoc :start (+ i-start offset (* frac weight)))
                (update :length #(* % weight)))))
        pats)
       (c/slice-starts start length))))
  (period [_] (let [ps    (map period pats)
                    top-p (weigh pats)]
                (* top-p (apply u/lcm ps))))
  (weight [_] 1))


(defrecord Times [n pat]
  Pattern
  (gen [_ start length]
    (-> (gen pat (* start n) (* n length))
        (c/scale (/ n))))
  (period [_] (period pat))
  (weight [_] (weight pat)))


(defrecord Elongate [x pat]
  Pattern
  (gen [_ start length] (gen pat start length))
  (period [_] (period pat))
  (weight [_] (* x (weight pat))))


(defrecord Repeat [n pat]
  Pattern
  (gen [_ start length] (gen (->Times n pat) start length))
  (period [_] 1)
  (weight [_] (* n (weight pat))))


(defrecord Splice [pats]
  Pattern
  (gen [_ start length] (gen (->Fit pats) start length))
  (period [_] 1) ;; ?
  (weight [_] (weigh pats)))


(defrecord Degrade [p pat]
  ;; TODO: Semi Deterministic in case we need to take in multiple slices?
  ;; And if so, how to ensure randomness when desired?
  Pattern
  (gen [_ start length]
    (map
     (fn [e]
       (if (< (rand) p) e
           (e/assoc-param :init nil)))
     (gen pat start length)))
  (period [_] (period pat))
  (weight [_] (weight pat)))


(defrecord Maybe [p pat]
  ;; TODO: Too deterministic?
  ;; And if so, how to ensure randomness when desired?
  Pattern
  (gen [_ start length]
    (let [pat-period (period pat)]
      (map
       (fn [e]
         (let [cycle-num (quot (e/start e) pat-period)
               keep?     (< (u/seeded-rand cycle-num) p)]
           (if keep? e (e/assoc-param e :init nil))))
       (gen pat start length))))
  (period [_] (period pat))
  (weight [_] (weight pat)))


(defrecord Pick [pats]
  ;; TODO: Semi Deterministic in case we need to take in multiple slices?
  ;; And if so, how to ensure randomness when desired?
  Pattern
  (gen [this start length]
    (let [p       (period this)
          i-start (iter-start start p)]
      (->
       (mapcat
        (fn [iter-no]
          (let [lucky (rand-nth pats)]
            (gen lucky iter-no p)))
        (iterate #(+ % p) i-start))
       (c/slice-starts start length))))
  (period [_] (period (first pats)))
  (weight [_] (weight (first pats))))


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


(defrecord Euclid [k n r pat]
  Pattern
  (gen [_ start length]
    (let [mask            (bjork (repeat k [true]) (repeat (- n k) [nil]))
          mask            (u/rot mask (or r 0))
          children        (map #(and % pat) mask)]
      (gen
       (->Fit (map ->pat children))
       start length)))
  (weight [_] 1)
  (period [_] 1))


(defrecord Reverse [pats])
(defrecord Rreverse [pats])


(defrecord Control [param value-tx pat]
  Pattern
  (gen [_ start length]
    (map (fn [e] (e/reassoc-param e :init param value-tx))
         (gen pat start length)))
  (period [_] (period pat))
  (weight [_] (weight pat)))


(defn by-iter
  [pats]
  (group-by (fn [p] (-> p spin first e/iter)) pats))


(defn re-weight
  [pats]
  (let [weights (map weight pats)]
    (if (every? #(= 1 %) weights)
      (mapcat spin pats)
      (mapcat
       (fn [[iter pats]]
         (let [weights          (map weight pats)
               cycls            (map spin pats)
               lengths          (map c/length cycls)
               weighted-lengths (u/weighted lengths weights)
               starts           (reductions + 0 weighted-lengths)]
           (mapcat
            (fn [c s l]
              (c/translate c (+ s iter) l))
            cycls
            starts
            weighted-lengths)))
       (by-iter pats)))))


(defn round-trip
  [op arg cycl mp]
  (let [start (c/start cycl)
        len   (c/length cycl)
        pat  (->Lit (c/translate cycl 0 1) 1 1)
        op   (op arg pat)]
    (-> op
        spin 
        (c/translate start len)
        (->Lit mp (weight op)))))


(defrecord OpMerge [op arg-pat val-pat]
  ;; TODO: This is gross
  Pattern
  (gen [_ start length]
    (let [arg-cycl (gen arg-pat start length)
          val-cycl (gen val-pat start length)
          mp       (long (Math/ceil (+ start length)))]
      (-> (map
           (fn [arg-evt]
             (let [arg     (e/get-init arg-evt)
                   overlap (c/slice-starts val-cycl (e/start arg-evt) (e/length arg-evt))]
               (round-trip op arg overlap mp)))
           arg-cycl)
          re-weight)))
  (period [_] (lcp [arg-pat val-pat]))
  (weight [_] (let [args (spin arg-pat)]
                (if (= 1 (count args))
                  (weight
                   (op (-> args first e/get-init) val-pat))
                  1))))



(defrecord EventMerge [merge-fn pats]
  Pattern
  (gen [_ start length]
    (let [cycls (map #(gen % start length) pats)]
      (reduce (fn [merged cycl] (m/merge-cycles merge-fn merged cycl)) cycls)))
  (period [_] (lcp pats))
  (weight [_] (apply max (map weight pats))))


(defrecord Stack [pats]
  Pattern
  (gen [_ start length]
    (mapcat #(gen % start length) pats))
  (period [_] (lcp pats))
  (weight [_] 1))


#_(defrecord ChopOp [n pat]
  Pattern
  (gen [_ start length]
    (let [evts      (gen pat start length)
          cnt       (count evts)
          op        (->Times (repeat n cnt) evts)
          pre-chops (operate op ctx)
          ;; ops       (map #(->TimesOp n [%]) evts)
          ;; pre-chops (mapcat #(operate % ctx) ops)
          ;; chops     (map #(-> %
          ;;                     (e/assoc-param :begin (:start %))
          ;;                     (e/assoc-param :end (e/end %)))
          ;;                pre-chops)
          ]
      
      pre-chops
      ;; evts
      ;; chops
      ))
  (period [_] (period pat))
  (weight [_] 1))

