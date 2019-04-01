(ns hopen.renderer.xf-test
  (:require #?(:clj  [clojure.test :refer [deftest testing is are]]
               :cljs [cljs.test    :refer [deftest testing is are]
                                   :include-macros true])
            [hopen.renderer.xf :refer [renderer default-env]]))

(deftest renderer-test

  (testing "basic testing"
    (let [env (update default-env :bindings assoc
                'square (fn [x] (* x x))
                'captain {:name "Captain", :age 41})
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

        ;; Inline quote
        [''a]              ['a]
        ['(quote a)]       ['a]
        ['(quote (a b c))] ['(a b c)]

        ;; Block if
        ['(b/if true  [1 2])]          [1 2]
        ['(b/if false [1 2])]          []
        ['(b/if true  [1 2] [3 4])]    [1 2]
        ['(b/if false [1 2] [3 4])]    [3 4]

        ;; Inline if and inline quote
        ['(+ 10 (if (= (hopen/ctx :foo) 'bar) 2 3))] [12]

        ;; Block cond
        [:x '(b/cond) :y]
        [:x :y]

        [:x '(b/cond
               (= (+ 2 2) 4) [:a :b]
               (= (+ 2 2) 4) [:c :d]
               :else [:something :else]) :y]
        [:x :a :b :y]

        [:x '(b/cond
               (not= (+ 2 2) 4) [:a :b]
               (= (+ 2 2) 4) [:c :d]
               :else [:something :else]) :y]
        [:x :c :d :y]

        [:x '(b/cond
               (not= (+ 2 2) 4) [:a :b]
               (not= (+ 2 2) 4) [:c :d]
               :else [:something :else]) :y]
        [:x :something :else :y]

        ;; Inline cond
        [:x '(cond) :y]
        [:x nil :y]

        [:x '(cond
               (not= (+ 2 2) 4) [:a :b]
               (= (+ 2 2) 4) [:c :d]
              :else [:something :else]) :y]
        [:x [:c :d] :y]

        ;; Block let
        ['(b/let [a 3 b (square a)]
            [a b a b])]
        [3 9 3 9]

        ;; Inline let
        ['(inc (let [a 2 b 3] (+ a b)))] [6]

        ;; Block for
        ['(b/for [a [:a :b :c]
                  b (range 1 3)]
            [a b])]
        [:a 1 :a 2 :b 1 :b 2 :c 1 :c 2]

        ['(b/for [a [:a :b :c] :separated-by ["|"]
                  b (range 1 3)]
            [a b])]
        [:a 1 :a 2 "|" :b 1 :b 2 "|" :c 1 :c 2]

        ['(b/for [a [:a :b :c] :separated-by ["|"]
                  b (range 1 3) :separated-by ["-"]]
            [a b])]
        [:a 1 "-" :a 2 "|" :b 1 "-" :b 2 "|" :c 1 "-" :c 2]

        ;; Inline for
        ['(conj (for [a [:a :b :c] b (range 2)] [a b]) :x)]
        [[[:a 0] [:a 1] [:b 0] [:b 1] [:c 0] [:c 1] :x]]

        ['(b/interpose ", " ["Alice" "Bernard" "Eugenie"])]
        ["Alice" ", " "Bernard" ", " "Eugenie"]

        ['(b/interpose (b/for [v [:sep1 :sep2]] [v])
                       [:x (b/for [a [:a :b] b [1 2]] [a b]) :y])]
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
    (let [env (update default-env :bindings assoc
                      'square (fn [x] (* x x))
                      'captain {:name "Captain", :age 41})
          template '[(hopen/ctx :n)
                     " * "
                     (hopen/ctx :n)
                     " = "
                     (square (hopen/ctx :n))]
          data {:n 3}]
      (is (= (into []
                   (renderer template env)
                   [data])
             [3
              " * "
              3
              " = "
              9]))))

  (testing "bindings"
    (let [template '["hello "
                     (b/let [person0 (get-in hopen/ctx [:person 0])
                             person1 (get-in hopen/ctx [:person 1])
                             person1-friend (person1 :friend)]
                       [(person0 :name)
                        ", "
                        (person1 :name)
                        " (whose friend is "
                        (person1-friend :name)
                        ") "
                        " and "
                        (hopen/root :name)])]
          data {:name "Alice"
                :person [{:name "Leonard"}
                         {:name "Albert"
                          :friend {:name "Eugenie"}}]}]
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
    (let [template '[(b/for [boy (hopen/ctx :boys)
                             girl (hopen/ctx :girls)
                             activity (hopen/ctx :activities)]
                       [(girl :name)
                        " "
                        activity
                        " with "
                        (boy :name)
                        :newline])]
          data {:boys [{:name "Albert"}
                       {:name "Leonard"}]
                :girls [{:name "Alice"}
                        {:name "Eugenie"}]
                :activities ["play SNES"
                             "learn Clojure"]}]
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
              "Eugenie learn Clojure with Leonard"]))))

  (testing "inner templates"
    (let [template '["Person list:\n"
                     (b/for [person (hopen/ctx :persons)]
                       ["- " (b/template :person person) "\n"])]
          person-template '["name: " (hopen/ctx :name) ", "
                            "age: " (hopen/ctx :age)]
          data {:persons [{:name "Alice", :age 25}
                          {:name "Leonard", :age 23}]}
          env (update default-env :templates assoc
                      :person person-template)]
      (is (= (into []
                   (renderer template env)
                   [data])
             ["Person list:\n"
              "- " "name: " "Alice" ", " "age: " 25 "\n"
              "- " "name: " "Leonard" ", " "age: " 23 "\n"]))))

  (testing "inner templates, parent-root"
    (let [root-template '["My root data is " hopen/root
                          " and my parent's root data is " hopen/parent-root ".\n"
                          "Inner-template:\n"
                          (b/template :inner-tpl (hopen/ctx :inner))]
          inner-template '["My root data is " hopen/root
                           " and my parent's root data is " hopen/parent-root ".\n"]
          data {:whoami :da-root
                :inner {:whoami :da-inner}}
          env (update default-env :templates assoc
                      :inner-tpl inner-template)]
      (is (= (into []
                   (renderer root-template env)
                   [data])
             ["My root data is " {:whoami :da-root
                                  :inner {:whoami :da-inner}}
              " and my parent's root data is " nil ".\n"
              "Inner-template:\n"
              "My root data is " {:whoami :da-inner}
              " and my parent's root data is " {:whoami :da-root
                                                :inner {:whoami :da-inner}} ".\n"])))))
