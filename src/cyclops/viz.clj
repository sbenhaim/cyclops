(ns cyclops.viz
  (:require
   [cyclops.events :as e]
   [cyclops.pattern :as p]
                                        ;[scicloj.kindly.v4.kind :as kind]
   [portal.viewer :as viewer]
   [cyclops.util :as u]))


(defn auto-param
  [[e & _]]
  (-> e
      (dissoc :period :start :length)
      keys
      first))


(defn vega-cycl
  ([cycl] (vega-cycl cycl :auto))
  ([cycl param]
   (let [events (e/realize cycl nil)
         param  (u/maybe-> param #{:auto} (fn [_] (auto-param events)))
         vals   (doall (for [e events :when (get e param)]
                         {:start (-> e :start float)
                          :end   (-> e e/end float)
                          :label (get e param)}))]

     {:description "(cyc|ops)",
      :data        {:values vals}
      :width       800

      :layer [{:mark     :bar
               :encoding {:y     {:field :label :type :ordinal}
                          :x     {:field :start :type :quantitative :axis {:values (map #(-> % :start float) events)}}
                          :x2    {:field :end}
                          :color {:field :label}}}
              {:mark     {:type     :text
                          :align    :left
                          :baseline :middle
                          :dy       0
                          :dx       5
                          :fontSize 16
                          :color    "white"}
               :encoding {:y    {:field :label :type :ordinal :title nil}
                          :x    {:field :start :type :quantitative}
                          :x2   {:field :end}
                          :text {:field :label :type :nominal}}}]})))



(comment
  (require '[cyclops.ops :refer :all])

  (tap>
   (pviz-cycl (p/->cycle (fit :a :b))))

  (n (cyc :a :e) :b [:c :d])
  )
