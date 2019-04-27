(ns hopen.syntax.util-test
  (:require #?(:clj  [clojure.test :refer [deftest testing is are]]
               :cljs [cljs.test    :refer [deftest testing is are]
                      :include-macros true])
            [hopen.syntax.util :refer [re-quote]]))

(deftest re-quote-test
  (are [input output]
    (= (re-quote input) output)

    "bonjour" "bonjour"
    "{{}}" "\\{\\{\\}\\}"
    "<%%>" "\\<%%>"))
