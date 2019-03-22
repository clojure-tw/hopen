(ns hopen.core-test
  (:require [clojure.test :refer :all]
            [hopen.core :refer :all]))

(deftest renderer-test

  (testing "getters"
    (let [data {:name "Alice"
                :person [{:name "Leonard"}
                         {:name "Albert"}]}
          template [[:value "hello "]
                    [:get 'hopen/ctx :name]
                    [:value " and "]
                    [:get-in 'hopen/ctx [:person 1 :name]]]]
      (is (= (into []
                   (renderer template)
                   [data])
             ["hello "
              "Alice"
              " and "
              "Albert"]))))

  (testing "functions"
    (let [data {:n 3}
          template [[:get 'hopen/ctx :n]
                    [:value " * "]
                    [:get 'hopen/ctx :n]
                    [:value " = "]
                    [:fn :square [:get 'hopen/ctx :n]]]
          fns {:square (fn [x] (* x x))}]
      (is (= (into []
                   (renderer template fns)
                   [data])
             [3
              " * "
              3
              " = "
              9])))))
