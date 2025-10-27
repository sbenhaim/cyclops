(ns cycl.viz
  (:require
   [cycl.events :as e]
   [cycl.pattern :as p]
                                        ;[scicloj.kindly.v4.kind :as kind]
   [cycl.util :as u]))


(defn auto-param
  [[e & _]]
  (-> e
      (dissoc :period :start :length)
      keys
      first))


(defn vega-cycl
  ([events] (vega-cycl events nil))
  ([events param]
   (let [param  (u/maybe-> param nil? (fn [_] (auto-param events)))
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
                          :color {:field :label :legend nil}}}
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
                          :text {:field :label :type :nominal}
                          :legend nil}}]})))

(comment
  (require '[cyclops.ops :refer :all])

  (tap>
   (pviz-cycl (p/->cycle (fit :a :b))))

  (n (cyc :a :e) :b [:c :d])
  )
