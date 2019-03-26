(ns hopen.core-test
  (:require #?(:clj  [clojure.test :refer [deftest testing is are]]
               :cljs [cljs.test    :refer [deftest testing is are]
                                   :include-macros true])
            [hopen.core :refer [renderer default-env]]))

(deftest renderer-test

  (testing "basic testing"
    (let [env (merge default-env
                     {'square (fn [x] (* x x))
                      'captain {:name "Captain", :age 41}})
          data {:foo 'bar}]
      (are [template output]
        (= (into [] (renderer template env) [data])
           output)

        ["hi"]                       ["hi"]
        [[1 2 3]]                    [[1 2 3]]
        ['captain]                   [{:name "Captain", :age 41}]
        ['(captain :name)]           ["Captain"]
        ['{5 captain, (square 3) 4}] [{5 {:name "Captain", :age 41}, 9 4}]
        ['(get-in captain [:name])]  ["Captain"]
        ['hopen/ctx]                 [{:foo 'bar}]
        ['(hopen/ctx :foo)]          ['bar]

        ;; Block quote
        ['(quote a)]                 ['a]
        [''a]                        ['a]
        ['(quote (a b c))]           ['(a b c)]

        ;; Block if
        ['(if true  [1 2])]          [1 2]
        ['(if false [1 2])]          []
        ['(if true  [1 2] [3 4])]    [1 2]
        ['(if false [1 2] [3 4])]    [3 4]

        ;; Inline if and inline quote
        ['(+ 10 (if (= (hopen/ctx :foo) 'bar) 2 3))] [12]

        ['(let [a 3 b (square a)]
            [a b a b])]
        [3 9 3 9]

        ['(for [a [:a :b :c] b (range 1 (+ 1 2))]
            [a b])]
        [:a 1 :a 2 :b 1 :b 2 :c 1 :c 2]

        ['(interpose ", " ["Alice" "Bernard" "Eugenie"])]
        ["Alice" ", " "Bernard" ", " "Eugenie"]

        ['(interpose (for [v [:sep1 :sep2]] [v])
                     [:x (for [a [:a :b] b [1 2]] [a b]) :y])]
        [:x
         :sep1 :sep2
         :a 1 :a 2 :b 1 :b 2
         :sep1 :sep2
         :y])))

 (testing "getters"
    (let [data {:name "Alice"
                :person [{:name "Leonard"}
                         {:name "Albert"}]}
          template '["hello "
                     (hopen/ctx :name)
                     " and "
                     (get-in hopen/ctx [:person 1 :name])]]
      (is (= (into []
                   (renderer template)
                   [data])
             ["hello "
              "Alice"
              " and "
              "Albert"]))))

  (testing "functions"
    (let [data {:n 3}
          template '[(hopen/ctx :n)
                     " * "
                     (hopen/ctx :n)
                     " = "
                     (square (hopen/ctx :n))]
          fns {'square (fn [x] (* x x))}]
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
          template '["hello "
                     (let [person0 (get-in hopen/ctx [:person 0])
                           person1 (get-in hopen/ctx [:person 1])
                           person1-friend (person1 :friend)]
                          [(person0 :name)
                           ", "
                           (person1 :name)
                           " (whose friend is "
                           (person1-friend :name)
                           ") "
                           " and "
                           (hopen/root :name)])]]
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
                :activities ["play SNES"
                             "learn Clojure"]}
          template '[(for [boy (hopen/ctx :boys)
                           girl (hopen/ctx :girls)
                           activity (hopen/ctx :activities)]
                          [(girl :name)
                           " "
                           activity
                           " with "
                           (boy :name)
                           :newline])]]
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
