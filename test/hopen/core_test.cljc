(ns hopen.core-test
  #?(:clj (:require [clojure.test :refer :all]
                    [hopen.core :refer :all])
     :cljs (:require [cljs.test :include-macros true :refer [deftest testing is]]
                     [hopen.core :refer [renderer]])))

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
              9]))))

  (testing "bindings"
    (let [data {:name "Alice"
                :person [{:name "Leonard"}
                         {:name "Albert"
                          :friend {:name "Eugenie"}}]}
          template [[:value "hello "]
                    [:let ['person0 [:get-in 'hopen/ctx [:person 0]]
                           'person1 [:get-in 'hopen/ctx [:person 1]]
                           'person1-friend [:get 'person1 :friend]]
                          [[:get 'person0 :name]
                           [:value ", "]
                           [:get 'person1 :name]
                           [:value " (whose friend is "]
                           [:get 'person1-friend :name]
                           [:value ") "]
                           [:value " and "]
                           [:get 'hopen/root :name]]]]]
      (is (= (into []
                   (renderer template)
                   [data])
             ["hello "
              "Leonard"
              ", "
              "Albert"
              " (whose friend is "
              "Eugenie"
              ") "
              " and "
              "Alice"]))))

  (testing "for loops"
    (let [data {:boys [{:name "Albert"}
                       {:name "Leonard"}]
                :girls [{:name "Alice"}
                        {:name "Eugenie"}]
                :activities [{:name "play SNES"}
                             {:name "learn Clojure"}]}
          ;; TODO: remove :name everywhere and use direct values with [:value symb]
          template [[:for ['boy [:get 'hopen/ctx :boys]
                           'girl [:get 'hopen/ctx :girls]
                           'activity [:get 'hopen/ctx :activities]]
                          [[:get 'girl :name]
                           [:value " "]
                           [:get 'activity :name]
                           [:value " with "]
                           [:get 'boy :name]
                           [:value :newline]]]]]
      (is (= (into []
                   (comp
                     (renderer template)
                     (partition-by #{:newline})
                     (partition-all 2)
                     (map #(apply str (first %))))
                   [data])
             ["Alice play SNES with Albert"
              "Alice learn Clojure with Albert"
              "Eugenie play SNES with Albert"
              "Eugenie learn Clojure with Albert"
              "Alice play SNES with Leonard"
              "Alice learn Clojure with Leonard"
              "Eugenie play SNES with Leonard"
              "Eugenie learn Clojure with Leonard"])))))
