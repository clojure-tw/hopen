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

      ;; An example from the Handlebars website.
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
             |</div>")]

      ""
      []

      "aa {{= < > =}} bb"
      ["aa  bb"]

      ;"aa {{! blabla }} bb"
      ;["aa  bb"]
      ;
      ;"aa {{!-- bla\nbla --}} bb"
      ;["aa  bb"]

      "aa {{= | | =}} bb |cc.dd.ee| ff"
      '["aa  bb " (get-in hopen/ctx [:cc :dd :ee]) " ff"]

      "{{foo bar a.b}}"
      '[(foo (hopen/ctx :bar) (get-in hopen/ctx [:a :b]))]

      ;"{{foo bar (a b.c)}}"
      ;'[(foo (hopen/ctx :bar) (a (hopen/ctx [:b :c])))]
      ;
      ;"{{foo bar a.b c=d e=f}}"
      ;'[(foo (hopen/ctx :bar)
      ;       (get-in hopen/ctx [:a :b])
      ;       {:c d, :e f})]

      "{{#if a.b}}c{{/if}}"
      '[(b/if (get-in hopen/ctx [:a :b])
          ["c"])]

      ;"{{#if a.b}}c{{else}}d{{/if}}"
      ;'[(b/if (get-in hopen/ctx [:a :b])
      ;    ["c"]
      ;    ["d"])]
      ;
      ;"{{#if a.b}}c{{else if d}}e{{/if}}"
      ;'[(b/if (get-in hopen/ctx [:a :b])
      ;    ["c"]
      ;    [(b/if (hopen/ctx :d)
      ;       ["e"])])]

      "{{#each a.b}}c{{/each}}"
      '[(b/for [hopen/ctx (get-in hopen/ctx [:a :b])]
          ["c"])]

      "{{#with a}}b{{/with}}"
      '[(b/let [hopen/ctx (hopen/ctx :a)]
          ["b"])]

      "{{#with a.b}}c{{/with}}"
      '[(b/let [hopen/ctx (get-in hopen/ctx [:a :b])]
          ["c"])]

      "aa {{#if bb}} cc {{#each dd.dd}} ee {{/each}} ff {{/if}} gg"
      '["aa "
        (b/if (hopen/ctx :bb)
          [" cc "
           (b/for [hopen/ctx (get-in hopen/ctx [:dd :dd])]
             [" ee "])
           " ff "])
        " gg"])))
