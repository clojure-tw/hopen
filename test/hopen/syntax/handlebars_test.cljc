(ns hopen.syntax.handlebars-test
  (:require #?(:clj  [clojure.test :refer [deftest testing is are]]
               :cljs [cljs.test    :refer [deftest testing is are]
                                   :include-macros true])
            [hopen.util :refer [triml]]
            [hopen.syntax.handlebars :as hb :refer [parse]]
            [instaparse.gll :refer [text->segment]]))

(deftest parse-change-delim-test
  (is (= (#'hb/parse-change-delim "= < > =}}blah blah" "}}")
         ["= < > =}}" "<" ">"])))

(deftest parse-text-segments-test
  (let [tx (fn [[text-segments next-segment << >>]]
             [(when text-segments (into (empty text-segments) (map str) text-segments))
              (when next-segment (str next-segment))
              <<
              >>])]
    (are [template expected-result]
      (= (tx (#'hb/parse-text-segments (text->segment template) "{{" "}}"))
         (tx expected-result))

      ""
      [[] nil "{{" "}}"]

      "{{ aa }} bb"
      [[] " aa }} bb" "{{" "}}"]

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
      (= (tx (#'hb/parse-syntax-segment (text->segment syntax-subs) "}}"))
         (tx expected-result))
      "aoeu}}"          ["aoeu" ""]
      "aoeu}}blah blah" ["aoeu" "blah blah"]))

  (is (thrown? #?(:clj Exception, :cljs js/Error)
               (#'hb/parse-syntax-segment (text->segment "aoeu") "}}"))))

(deftest parse-test
  (testing "Parser's conformity"
    (are [text-template data-template]
      (= (parse text-template) data-template)

      "Hello, world." ["Hello, world."]

      (triml "<div class=\"entry\">
             |  <h1>{{title}}</h1>
             |  <div class=\"body\">
             |    {{body}}
             |  </div>
             |</div>")
      [(triml "<div class=\"entry\">
              |  <h1>")
       '(hopen/ctx :title)
       (triml "</h1>
              |  <div class=\"body\">
              |    ")
       '(hopen/ctx :body)
       (triml "
              |  </div>
              |</div>")]

      (triml "<div class=\"entry\">
             |  <h1>{{title}}</h1>
             |  <h2>By {{author.name}}</h2>
             |
             |  <div class=\"body\">
             |    {{body}}
             |  </div>
             |</div>")
      [(triml "<div class=\"entry\">
             |  <h1>")
       '(hopen/ctx :title)
       (triml "</h1>
             |  <h2>By ")
       '(get-in hopen/ctx [:author :name])
       (triml "</h2>
             |
             |  <div class=\"body\">
             |    ")
       '(hopen/ctx :body)
       (triml "
             |  </div>
             |</div>")])))
