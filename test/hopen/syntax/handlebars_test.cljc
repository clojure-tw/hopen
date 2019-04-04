(ns hopen.syntax.handlebars-test
  (:require #?(:clj  [clojure.test :refer [deftest testing is are]]
               :cljs [cljs.test    :refer [deftest testing is are]
                                   :include-macros true])
            [hopen.syntax.handlebars :refer [parser]]))

(deftest parser-test
  (testing "Parser's conformity"
    (are [text-template data-template]
      (= (parser text-template) data-template)

      "Hello, world." ["Hello, world."])))
