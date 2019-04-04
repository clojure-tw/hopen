(ns hopen.util-test
  (:require #?(:clj  [clojure.test :refer [deftest testing is are]]
               :cljs [cljs.test    :refer [deftest testing is are]
                      :include-macros true])
            [hopen.util :refer [triml parse-bindings collect collect-in]]))

(deftest triml-test
  (is (= (triml "hello,
                | world!")
         "hello,\n world!")))

(deftest parse-bindings-test
  (testing "Example-spec the function's input and output"
    (are [input output]
      (= (parse-bindings input) output)

      (seq [])
      []

      (seq '[var0 val0 :foo foo-val :bar bar-val
             var1 val1
             var2 val2 :separated-by "|"])
      '[[var0 val0 {:foo foo-val :bar bar-val}]
        [var1 val1 nil]
        [var2 val2 {:separated-by "|"}]])))

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
