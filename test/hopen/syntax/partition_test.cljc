(ns hopen.syntax.partition-test
  (:require #?(:clj  [clojure.test :refer [deftest testing is are]]
               :cljs [cljs.test    :refer [deftest testing is are]
                      :include-macros true])
            [hopen.syntax.partition :as part]
            [instaparse.gll :refer [text->segment]]))

(deftest re-matches-test
  (testing "Check that some edge cases on regexp are consistent across the platforms."
    (are [re s]
      (re-matches re s)

      #"\!\s+([\s\S]*)\s" "! blabla "
      #"\!\s+([\s\S]*)\s" "! bla\nbla "
      #"\!\s+([\s\S]*)\s" "!\nbla\nbla\n")))

(deftest parse-change-delim-test
  (is (= (#'part/parse-change-delim "= < > =}}blah blah" "}}")
         ["= < > =}}" "<" ">"])))

(deftest parse-text-segments-test
  (let [tx (fn [[text-segments next-segment << >>]]
             [(when text-segments (into (empty text-segments) (map str) text-segments))
              (when next-segment (str next-segment))
              <<
              >>])]
    (are [template expected-result]
      (= (tx (#'part/parse-text-segments (text->segment template) "{{" "}}"))
         (tx expected-result))

      ""
      [[""] nil "{{" "}}"]

      "{{ aa }} bb"
      [[""] " aa }} bb" "{{" "}}"]

      "aa {{ bb }} cc"
      [["aa "] " bb }} cc" "{{" "}}"]

      "aa {{= { } =}} bb { cc } dd"
      [["aa " " bb "] " cc } dd" "{" "}"]

      "aa {{= {{{ }}} =}} bb"
      [["aa " " bb"] nil "{{{" "}}}"])))

(deftest parse-syntax-segment-test
  (let [tx (fn [[syntax-segment next-segment]]
             [(when syntax-segment (str syntax-segment))
              (when next-segment (str next-segment))])]
    (are [syntax-subs expected-result]
      (= (tx (#'part/parse-syntax-segment (text->segment syntax-subs) "}}"))
         (tx expected-result))
      "aoeu}}"          ["aoeu" ""]
      "aoeu}}blah blah" ["aoeu" "blah blah"]))

  (is (thrown? #?(:clj Exception, :cljs js/Error)
               (#'part/parse-syntax-segment (text->segment "aoeu") "}}"))))
