(ns hopen.syntax.mustache-test
  (:require [hopen.syntax.mustache :as sut]
            [hopen.renderer.xf]
            #?(:clj  [clojure.test :as t :refer [deftest testing is]]
               :cljs [cljs.test    :as t :refer [deftest testing is]
                      :include-macros true])))

(defn- render [template data]
  (let [data-template (#'sut/parse template)]
    (->> (into [] (hopen.renderer.xf/renderer data-template (#'sut/init-env hopen.renderer.xf/default-env data))
               [data])
         (apply str))))

(deftest tag-parsing
  (testing "re-seq-pos parity"

    ;; `re-seq-pos` is implemented differently between clj and cljs
    ;; Make sure they are behaving the same
    (is (= (#'sut/re-seq-pos
            (#'sut/compile-regex "{{" "}}")
"* {{name}}
* {{age}}
* {{company}}
* {{{company}}}
")

         [{:start 2, :end 10, :groups ["{{name}}" nil "name"]}
          {:start 13, :end 20, :groups ["{{age}}" nil "age"]}
          {:start 23, :end 34, :groups ["{{company}}" nil "company"]}
          {:start 37, :end 50, :groups ["{{{company}}}" "company" nil]}])))

  (testing "Deliminator parsing"
    (is (= (#'sut/parse-delim-change-tag "={{ }}=") '("{{" "}}")))
    (is (= (#'sut/parse-delim-change-tag "=<% %>=") '("<%" "%>"))))

  (testing "Exercise all tag types"
    (is (= (#'sut/retrieve-all-tags

;; Do not change indentation of this string
;; The test result includes counting exact offset into the string
"
 * {{name}}
 * {{age}}
 * {{company}}
 * {{{company}}}

 Shown.
 {{#person}}
    never shown
 {{/person}}


 {{#wrapped}}
   {{name}} is awesome.
 {{/wrapped}}


 {{#repo}}
   <b>{{name}}</b>
 {{/repo}}
 {{^repo}}
   No repos :(
 {{/repo}}

 {{> next_more}}
")
           [{:content "name", :start 4, :end 12, :groups ["{{name}}" nil "name"]}
            {:content "age", :start 16, :end 23, :groups ["{{age}}" nil "age"]}
            {:content "company", :start 27, :end 38, :groups ["{{company}}" nil "company"]}
            {:content "company", :escape true, :start 42, :end 55, :groups ["{{{company}}}" "company" nil]}
            {:content "#person", :start 66, :end 77, :groups ["{{#person}}" nil "#person"]}
            {:content "/person", :start 95, :end 106, :groups ["{{/person}}" nil "/person"]}
            {:content "#wrapped", :start 110, :end 122, :groups ["{{#wrapped}}" nil "#wrapped"]}
            {:content "name", :start 126, :end 134, :groups ["{{name}}" nil "name"]}
            {:content "/wrapped", :start 148, :end 160, :groups ["{{/wrapped}}" nil "/wrapped"]}
            {:content "#repo", :start 164, :end 173, :groups ["{{#repo}}" nil "#repo"]}
            {:content "name" :start 180, :end 188, :groups ["{{name}}" nil "name"]}
            {:content "/repo" :start 194, :end 203, :groups ["{{/repo}}" nil "/repo"]}
            {:content "^repo":start 205, :end 214, :groups ["{{^repo}}" nil "^repo"]}
            {:content "/repo" :start 231, :end 240, :groups ["{{/repo}}" nil "/repo"]}
            {:content "> next_more" :start 243, :end 258, :groups ["{{> next_more}}" nil "> next_more"]}])))

  (testing "tag collection with delim change"
    (is (= (#'sut/retrieve-all-tags

"
 * {{default_tags}}
 {{=<% %>=}}
 * <% erb_style_tags %>
 <%={{ }}=%>
 * {{ default_tags_again }}
")

           [{:content "default_tags", :start 4, :end 20, :groups ["{{default_tags}}" nil "default_tags"]}
            {:content "=<% %>=",
             :start 22,
             :end 33,
             :groups ["{{=<% %>=}}" nil "=<% %>="],
             :delims ["<%" "%>"],
             :type :delim-change}
            {:content " erb_style_tags ", :start 37, :end 57, :groups ["<% erb_style_tags %>" nil " erb_style_tags "]}
            {:content "={{ }}=",
             :start 59,
             :end 70,
             :groups ["<%={{ }}=%>" nil "={{ }}="],
             :delims ["{{" "}}"],
             :type :delim-change}
            {:content " default_tags_again ",
             :start 74,
             :end 98,
             :groups ["{{ default_tags_again }}" nil " default_tags_again "]}])))

  (testing "tag block grouping"
    (let [text

"
 {{something}}

 {{#movies}}
   {{#actors}}
     {{name}}
   {{/actors}}
 {{/movie}}

 {{something}}
"]
      (is (= (->> text
                  (#'sut/retrieve-all-tags)
                  (map #'sut/assoc-tag-type)
                  (#'sut/tags->ast))
             [{:content "something"
               :start 2,
               :end 15,
               :groups ["{{something}}" nil "something"],
               :type :var-ref,
               :var-name "something"}
              [{:content "#movies"
                :start 18,
                :end 29,
                :groups ["{{#movies}}" nil "#movies"],
                :type :section-open,
                :section-name "movies"}
               [{:content "#actors"
                 :start 33,
                 :end 44,
                 :groups ["{{#actors}}" nil "#actors"],
                 :type :section-open,
                 :section-name "actors"}
                {:content "name"
                 :start 50,
                 :end 58,
                 :groups ["{{name}}" nil "name"],
                 :type :var-ref,
                 :var-name "name"}
                {:content "/actors"
                 :start 62,
                 :end 73,
                 :groups ["{{/actors}}" nil "/actors"],
                 :type :section-close,
                 :section-name "actors"}]
               {:content "/movie"
                :start 75,
                :end 85,
                :groups ["{{/movie}}" nil "/movie"],
                :type :section-close,
                :section-name "movie"}]
              {:content "something"
               :start 88,
               :end 101,
               :groups ["{{something}}" nil "something"],
               :type :var-ref,
               :var-name "something"}]))))

  (testing "tag lineno retrieval"
    (let [text
"{{#repo}}
something goes
here
and there
{{/repo}}
"]
      (is (= (as-> text $
               (#'sut/retrieve-all-tags $)
               (#'sut/join-tag-lineno $ text))

             [{:content "#repo", :start 0, :end 9, :groups ["{{#repo}}" nil "#repo"], :lineno 1}
              {:content "/repo", :start 40, :end 49, :groups ["{{/repo}}" nil "/repo"], :lineno 5}]))))

  (testing "multiple tags in single line"
    (let [text
          "{{#repo}} something {{/repo}} {{{escape this text}}}"]

      (is (= (as-> text $
               (#'sut/retrieve-all-tags $)
               (#'sut/join-tag-lineno $ text))

             [{:start 0,
               :end 9,
               :groups ["{{#repo}}" nil "#repo"],
               :content "#repo",
               :lineno 1}
              {:start 20,
               :end 29,
               :groups ["{{/repo}}" nil "/repo"],
               :content "/repo",
               :lineno 1}
              {:start 30,
               :end 52,
               :groups ["{{{escape this text}}}" "escape this text" nil],
               :content "escape this text",
               :escape true,
               :lineno 1}])))))

(deftest feature-validation
  (testing "False values or Empty lists"
    (let [template "Shown.
{{#person}}
Never shown!
{{/person}}"]
      (is (= (render template {:person false})
             "Shown.\n"))

      (is (= (render template {})
             "Shown.\n"))))

  (testing "Non-Empty Lists"
    (is (= (render "{{#repo}}
<b>{{name}}</b>
{{/repo}}
"
                         {:repo [{:name "resque"}
                                 {:name "hub"}
                                 {:name "rip"}]})

           "<b>resque</b>\n<b>hub</b>\n<b>rip</b>\n"))

    (is (= (render "# Movies list
{{#movies}}
## {{name}}
### Actors
{{#cast}}
* {{name}}
{{/cast}}
{{/movies}}
"
                 {:movies [{:name "Tropic Thunder"
                            :cast [{:name "Ben Stiller"}
                                   {:name "RDJ"}
                                   {:name "Jack Black"}]}
                           {:name "The secret life of Walter Mitty"
                            :cast [{:name "Ben Stiller"}
                                   {:name "Kristen Wiig"}]}]})

           "# Movies list\n## Tropic Thunder\n### Actors\n* Ben Stiller\n* RDJ\n* Jack Black\n## The secret life of Walter Mitty\n### Actors\n* Ben Stiller\n* Kristen Wiig\n")))

  (testing "Non-False Values"
    (= (render "{{#person?}}
Hi {{name}}!
{{/person?}}
"
                     {:person? {:name "Jon"}})

       "Hi Jon!\n"))

  (testing "Inverted Sections"
    (is (=
         (render "{{#repo}}
<b>{{name}}</b>
{{/repo}}
{{^repo}}
No repos :(
{{/repo}}
"
                       {:repo []})

         "No repos :(\n")))

  (testing "Comments"
    (is (= (render "<h1>Today{{! ignore me }}.</h1>" {})
           "<h1>Today.</h1>")))


  (testing "Set Delimiter"
    (is (= (render "* {{hello}}
{{=<% %>=}}
* <% world %>
<%={{ }}=%>
* {{again}}
"
                         {:hello "hello"
                          :world "world"
                          :again "again"})

           "* hello\n* world\n* again\n"))))


(deftest better-tag-parsing
  (testing "ignore whitespace"
    (let [text
"{{#   repo}}
<b>{{name}}</b>
{{ /  repo}}
"]

      (is (= (->> text
                  (#'sut/retrieve-all-tags)
                  (map #'sut/assoc-tag-type))

             [{:content "#   repo"
               :start 0,
               :end 12,
               :groups ["{{#   repo}}" nil "#   repo"],
               :type :section-open,
               :section-name "repo"}
              {:content "name"
               :start 16,
               :end 24,
               :groups ["{{name}}" nil "name"],
               :type :var-ref,
               :var-name "name"}
              {:content " /  repo"
               :start 29,
               :end 41,
               :groups ["{{ /  repo}}" nil " /  repo"],
               :type :section-close,
               :section-name "repo"}])))))

(deftest error-checking
  (testing "unclosed context detection"
    (is (thrown-with-msg? #?(:clj Exception
                             :cljs ExceptionInfo)
                          #"Section tag \"\{\{#hello\}\}\" opened on line 1 is never closed"
                 (render
"{{#hello}}
something
{{#world}}
"
                               {})))))
