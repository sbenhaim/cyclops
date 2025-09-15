# Document Title


;; How appropriate, going in loops
;; If `b` is allowed to be a function of 1 arg
;; How do we determine if it takes `ctx` or `a`
;; The previous idea was that it always takes `ctx`
;; And `ctx` conveniently contains `:prev`.
;;
;; Option 2 is that anything based on the `:prev` value
;; Takes 2 args `ctx` and `prev`.
;;
;; Option 3 swaps the order
;;
;; fn of `:prev`
;; 1: (fn [ctx] (inc (:prev ctx)))
;; 2: (fn [ctx prev] (inc prev))
;; 3: inc
;; 
;; Now do this for merging
;; 1. (fn [a b] (fn [ctx] (case (fn1? b) (b (assoc ctx :prev a))
;; 2. (fn [a b] (fn [ctx] (b ctx a)))
;; 3. (fn [a b] (fn [ctx] (case (arity b) 1 (b (real a)) 2 (b ctx (real a)) b)))
;;
;; fn of `ctx`
;; 1: ctx-fn
;; 2: ctx-prev-fn
;; 3: (fn [_ prev] (inc prev))
;; Now do this for merging
;; 1. (fn [a b] (fn [ctx] (if (fn+1? b) (b (realize a ctx)) (f (real a) (real b)))))
;; 2. (fn [a b] (fn [ctx] (if (fn+2? b) (b ctx (real a)) b)))
;; 3. (fn [a b] (fn [ctx] (case (arity b) 1 (b (real a)) 2 (b ctx (real a)) b)))
;;
;; Decision: No tx stack
;; Realization handled by merge fn wrappers
;;
;; Open question
;; Should user-provided fns
;; 1. Take only a single ctx arg, which is appropriately populated by merge
;; 2. Take 1 or 2 args, ctx first
;; 3. Take 1 or 2 args, prev first
;;
;; 1. User Apply: (fn [ctx] (inc (:prev ctx)))
;; 1. User Ctx: (fn [ctx] (sine ctx))
;; 1. User Both: (fn [ctx] (sine ctx (:prev ctx)))
;; 1. Merge: (fn [b a] (fn [ctx] (cond (fn1? b) (b (assoc ctx :prev (realize a ctx))) (f (real a ctx) (real b ctx)))))
;;
;; 2. User Apply: (fn [_ prev] (inc prev))
;; 2. User Ctx: sine
;; 2. User Both: (fn [ctx prev] (sine ctx prev))
;; 2. Merge: (fn [b a] (fn [ctx] (cond (fn2? b) (b ctx (realize a ctx)) :else (f (real a ctx) (real b ctx)))))
;; 
;; 3. User Apply: inc
;; 3. User Ctx: (fn [_ ctx] (sine ctx))
;; 3. User Both: (fn [prev ctx] (sine ctx prev))
;; 3. Merge: (fn [b a] (fn [ctx] (cond (fn1? b) (b (realize a ctx)) (fn2? b) (b ctx (realize a ctx)) :else (f (real a ctx) (real b ctx)))))
;;
;; 1. Pro: All fns take 1 arg. That's simple
;; 1. Con: Key naming convention
;; 1. Con: All fns uglier
;; 1. Con: Misdirection on merge
;;
;; 2: Pro: Simplest Merge fun
;; 2: Pro: Ctx funs are clean
;; 2: Con: Can't use built-ins, uglier
;;
;; 3: Pro: Can use built-in 1-arg fns
;; 3: Con: Ctx funs get uglier
;; 3: Con: SLIGHTLY more complex merge


