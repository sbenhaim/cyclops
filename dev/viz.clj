(ns viz
  (:require
   [cyclops.events :as e]
   [scicloj.kindly.v4.kind :as kind]
   [portal.viewer :as viewer]))


(do
  (defn pviz-cycl [cycl]
    (let [events (e/events cycl)
          vals   (doall (for [e events]
                          {:start (-> e :start float)
                           :end   (-> e e/end float)
                           :n     (-> e :params :n)}))]
      (viewer/vega-lite
       {:description "(cyc|ops)",
        :data        {:values vals}
        :mark        :bar
        :encoding    {:y  {:field :n :type :ordinal}
                      :x  {:field :start :type :quantitative :title :n :axis {:values (map #(-> % :start float) events)}}
                      :x2 {:field :end}}})))

  (kind/vega-lite
   (pviz-cycl
    (n [:a :b] [:c :d]))))


(viewer/vega-lite
 {:description "(cyc|ops)",
  :data        {:values vals}
  :mark        :bar
  :encoding    {:y  {:field :n :type :ordinal}
                :x  {:field :start :type :quantitative :title :n :axis {:values (map #(-> % :start float) events)}}
                :x2 {:field :end}}})


(tagged-literal 'flare/html (kind/hiccup [:h1 "Yoh"]))

(kind/hiccup [:h1 "Yoh"])

(kind/hiccup
 [:h1 "Yo"])

(comment
  (require '[cyclops.ops :refer :all])

  (n (cyc :a :e) :b [:c :d])
  )
