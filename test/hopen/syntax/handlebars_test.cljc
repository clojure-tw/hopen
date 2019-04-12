(ns hopen.syntax.handlebars-test
  (:require #?(:clj  [clojure.test :refer [deftest testing is are]]
               :cljs [cljs.test    :refer [deftest testing is are]
                                   :include-macros true])
            [hopen.util :refer [triml]]
            [hopen.syntax.handlebars :as hb :refer [parse]]
            [instaparse.gll :refer [text->segment]]))

(deftest re-matches-test
  (testing "Check that some edge cases on regexp are consistent across the platforms."
    (are [re s]
      (re-matches re s)

      #"\!\s+([\s\S]*)\s" "! blabla "
      #"\!\s+([\s\S]*)\s" "! bla\nbla "
      #"\!\s+([\s\S]*)\s" "!\nbla\nbla\n")))

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

      "aa {{! blabla }} bb"
      ["aa  bb"]

      "aa {{! bla\nbla }} bb"
      ["aa  bb"]

      "aa {{!-- blabla --}} bb"
      ["aa  bb"]

      "aa {{!-- bla\nbla --}} bb"
      ["aa  bb"]

      "aa {{= | | =}} bb |cc.dd.ee| ff"
      '["aa  bb " (get-in hopen/ctx [:cc :dd :ee]) " ff"]

      "{{false}}{{true}}"
      [false true]

      "{{0}}{{3}}{{-0}}{{-3}}"
      [0 3 -0 -3]

      "{{ \"some text\" }}"
      ["some text"]

      "{{foo bar a.b}}"
      '[(foo (hopen/ctx :bar) (get-in hopen/ctx [:a :b]))]

      "{{foo bar (a b.c)}}"
      '[(foo (hopen/ctx :bar) (a (get-in hopen/ctx [:b :c])))]

      "{{foo bar a.b c=d e=true f=3 g=\"hello\"}}"
      '[(foo (hopen/ctx :bar)
             (get-in hopen/ctx [:a :b])
             {:c (hopen/ctx :d)
              :e true
              :f 3
              :g "hello"})]

      "a{{#if b}}c{{/if}}d"
      '["a"
        (b/if (hb/true? (hopen/ctx :b))
          ["c"])
        "d"]

      "a{{#if b}}c{{else}}d{{/if}}e"
      '["a"
        (b/if (hb/true? (hopen/ctx :b))
          ["c"]
          ["d"])
        "e"]

      "a{{#if b}}c{{else if d}}e{{/if}}f"
      '["a"
        (b/if (hb/true? (hopen/ctx :b))
          ["c"]
          [(b/if (hb/true? (hopen/ctx :d))
             ["e"])])
        "f"]

      "a{{#if b}}c{{else if d}}e{{else}}f{{/if}}g"
      '["a"
        (b/if (hb/true? (hopen/ctx :b))
          ["c"]
          [(b/if (hb/true? (hopen/ctx :d))
             ["e"]
             ["f"])])
        "g"]

      "a{{#unless b}}c{{/unless}}d"
      '["a"
        (b/if (hb/false? (hopen/ctx :b))
          ["c"])
        "d"]

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
        (b/if (hb/true? (hopen/ctx :bb))
          [" cc "
           (b/for [hopen/ctx (get-in hopen/ctx [:dd :dd])]
             [" ee "])
           " ff "])
        " gg"]

      "{{#each coll as |x i|}}d{{/each}}"
      '[(b/for [x (hopen/ctx :coll) :indexed-by i]
         ["d"])]

      "a {{> confirm-button}} b"
      '["a "
        (b/template :confirm-button hopen/ctx)
        " b"]

      "a {{> confirm-button title=\"Alright\"}} b"
      '["a "
        (b/template :confirm-button (merge hopen/ctx {:title "Alright"}))
        " b"])))

