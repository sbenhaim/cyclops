(ns cyclops.types
  (:require
   [cyclops.events :as e]
   [cyclops.pattern :as p]
   [malli.core :as m]))


(declare TValue)
(declare TEvent)

(def TRealizeContext
  (m/schema [:map
             [:event TEvent]
             [:cycle-num number?] ;; Or is it int?
             [:layer {:optional true} int?]
             ]))


;; Just in case we constrain later
(def TRawValue
  (m/schema [:enum number? keyword? string?])) 


(def TStackedValue
  (m/schema [:set TRawValue]))


(def TRealizedValue
  (m/schema [:enum TRawValue TStackedValue]))



(def =>TValueFn
  (m/schema
   [:function
    [:=> [:cat] TRealizedValue]
    [:=> [:cat TValue] TRealizedValue]
    [:=> [:cat TValue TRealizeContext] TRealizedValue]]))


(def TValue
  (m/schema [:enum =>TValueFn TRealizedValue TStackedValue TRawValue]))



(def TEvent (m/schema [:map
                       [:start number?]
                       [:length number?]
                       [:period number?]
                       [:params :map]]))


(comment
  (m/validate TEvent (e/->Event {:n :bd} 0 1 1))
  (m/validate TEvent (e/->event :a 0 1 1))
  (m/validate TEvent {:start 0 :length 1 :period 1 :params {}})
  (m/validate TEvent {:start 0 :length 1 :period 1})
  ,)


(def TCycle (m/schema [:sequential TEvent]))

(comment
  (m/validate TCycle [(e/->event :a 0 1 1)])
  (m/validate TCycle [{:start 0 :length 1 :period 1 :params {}}])
  (m/validate TCycle [{:start 0 :period 1 :params {}}])
  ,)


(def TOp
  (m/-simple-schema
   {:type :cyclops/op
    :pred p/op?
    :type-properties {:error/message "Must implement Operatic protocol"}}))


(comment
  (m/validate TOp [:a :b :c])
  (m/validate TOp (p/->TimesOp 3 [:a]))
  (m/validate TOp #{:a :b :c}))



(def TPatternEntry
  [:enum TValue TEvent map? TOp])


(def TPattern
  [:sequential TPatternEntry])


(def TArgPattern
  [:sequential [:enum TValue TOp]])


(def =>TOp1
  (m/schema
   [:function
    [:=> [:cat [:enum TPattern [:* TPatternEntry]]] TOp]]))


(def =>TOp*
  (m/schema
   [:function
    [:=> [:cat TArgPattern [:enum TPattern [:* TPatternEntry]]] TCycle]]))



(def =>OpTx
  (m/schema
   [:function
    [:=> [:cat :sequential ]]]))
