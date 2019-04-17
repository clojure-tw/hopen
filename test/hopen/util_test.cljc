(ns hopen.util-test
  (:require #?(:clj  [clojure.test :refer [deftest testing is are]]
               :cljs [cljs.test    :refer [deftest testing is are]
                      :include-macros true])
            [hopen.util :refer [triml parse-bindings]]))

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
