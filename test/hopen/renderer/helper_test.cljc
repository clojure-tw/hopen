(ns hopen.renderer.helper-test
  (:require #?(:clj  [clojure.test :refer [deftest testing is are]]
               :cljs [cljs.test    :refer [deftest testing is are]
                      :include-macros true])
            [hopen.renderer.helper :refer [collect collect-in]]))

(deftest collect-test
  (testing "Example-spec the function's input and output"
    (are [data keys output]
      (= (collect data keys) output)

      nil              [:a :c :d]  []

      {:a 1 :b 2 :c 3} []          []
      {:a 1 :b 2 :c 3} [:a :c :d] [1 3]

      [:a :b :c :d]    []          []
      [:a :b :c :d]    [0 2 7]     [:a :c])))

(deftest collect-in-test
  (testing "Example-spec the function's input and output"
    (are [data path output]
      (= (collect-in data path) output)

      nil        [[0 2 7]] []

      [:a :b :c] [[]]      []
      [:a :b :c] []        nil

      [{:a 1    :b 2    :c 3}
       {:a 10   :b 20   :c 30}
       {:a 100  :b 200  :c 300}
       {:a 1000 :b 2000 :c 3000}]
      [[0 2 7] [:a :c :d]]
      [1 3 100 300])))
