(ns types
  (:require [malli.core :as m]
            [cyclops.pattern :as p]
            [cyclops.events :as e]))

;; Op is a tree with ops for branches, and values at the leaves
;; Values can be any type, with special semantics for:
;; - scalars: base values, become event params for making music
;; - vectors: stacked values, played simultaneously
;; - maps: higher level of event control by providing explicit params
;; - events: highest level of event control, but also allows ops to operate on cycls


;; Naked events are events with only an :init param.
;; Naked cycls are collections of naked events


;; Ctrls convert naked cycls into semantic cycls that have musical meaning


;; Ops vs Ctrls
;; Ops operate on values and apply or overwrite timing. Ignorant of event semantics other than timing.
;; Ctrls operate on cycls. Affect event semantics, i.e., reify naked events or adjust existing timing.


(def t-op (m/schema [:=>
                     [:cat [:sequential :any]]
                     p/Operatic])) ;; Or p/Op

(def t-op-v2 (m/schema [:=>
                        [:cat p/Operatic]
                        p/Operatic])) ;; Or p/Op


(def t-cycl [:sequential e/Event])

(def t-ctrl (m/schema [:=> [:cat t-cycl]]))

;; Which is better, given that [:sequential :any] is itself p/Operatic?


(m/schema [:sequential :any])

(comment
  (fit [{:start 1/2 :length 1/2}]))


(def t-pat-op (m/schema [:=> [:cat p/Operatic [:sequential :any]]
                         p/Operatic])) ;; Or p/Op
