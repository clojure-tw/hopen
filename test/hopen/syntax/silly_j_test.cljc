(ns hopen.syntax.silly-j-test
  (:require #?(:clj [clojure.test :refer [deftest testing is are]]
               :cljs [cljs.test :refer [deftest testing is are]
                      :include-macros true])
            [hopen.syntax.silly-j :refer [parse]]))

(deftest parse-test
  (testing "basic syntax test"
    (are [template parsed]
        (= (parse template)
           parsed)
      ;; Split string and context
        "Hello {@:name}, {@:n} * {@:n} = {square @:n}"                ["Hello "
                                                                       '(hopen/ctx :name)
                                                                       ", "
                                                                       '(hopen/ctx :n)
                                                                       " * "
                                                                       '(hopen/ctx :n)
                                                                       " = "
                                                                       '(square (hopen/ctx :n))]
        )))
