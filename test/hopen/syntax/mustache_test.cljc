(ns hopen.syntax.mustache-test
  (:require [hopen.syntax.mustache :as sut]
            #?(:clj [clojure.test :as t :refer [deftest testing is]]
               :cljs [cljs.test :as t :include-macros true])))

(deftest tag-parsing
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
           [{:start 4, :end 12, :groups ["{{name}}" "name"]}
            {:start 16, :end 23, :groups ["{{age}}" "age"]}
            {:start 27, :end 38, :groups ["{{company}}" "company"]}
            {:start 42, :end 55, :groups ["{{{company}}}" "{company}"]}
            {:start 66, :end 77, :groups ["{{#person}}" "#person"]}
            {:start 95, :end 106, :groups ["{{/person}}" "/person"]}
            {:start 110, :end 122, :groups ["{{#wrapped}}" "#wrapped"]}
            {:start 126, :end 134, :groups ["{{name}}" "name"]}
            {:start 148, :end 160, :groups ["{{/wrapped}}" "/wrapped"]}
            {:start 164, :end 173, :groups ["{{#repo}}" "#repo"]}
            {:start 180, :end 188, :groups ["{{name}}" "name"]}
            {:start 194, :end 203, :groups ["{{/repo}}" "/repo"]}
            {:start 205, :end 214, :groups ["{{^repo}}" "^repo"]}
            {:start 231, :end 240, :groups ["{{/repo}}" "/repo"]}
            {:start 243, :end 258, :groups ["{{> next_more}}" "> next_more"]}])))

  (testing "tag collection with delim change"
    (is (= (#'sut/retrieve-all-tags

"
 * {{default_tags}}
 {{=<% %>=}}
 * <% erb_style_tags %>
 <%={{ }}=%>
 * {{ default_tags_again }}
")

           [{:start 4, :end 20, :groups ["{{default_tags}}" "default_tags"]}
            {:start 22,
             :end 33,
             :groups ["{{=<% %>=}}" "=<% %>="],
             :delims ["<%" "%>"],
             :type :delim-change}
            {:start 37, :end 57, :groups ["<% erb_style_tags %>" " erb_style_tags "]}
            {:start 59,
             :end 70,
             :groups ["<%={{ }}=%>" "={{ }}="],
             :delims ["{{" "}}"],
             :type :delim-change}
            {:start 74,
             :end 98,
             :groups ["{{ default_tags_again }}" " default_tags_again "]}])))

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
             [{:start 2,
               :end 15,
               :groups ["{{something}}" "something"],
               :type :var-ref,
               :var-name "something"}
              [{:start 18,
                :end 29,
                :groups ["{{#movies}}" "#movies"],
                :type :section-open,
                :section-name "movies"}
               [{:start 33,
                 :end 44,
                 :groups ["{{#actors}}" "#actors"],
                 :type :section-open,
                 :section-name "actors"}
                {:start 50,
                 :end 58,
                 :groups ["{{name}}" "name"],
                 :type :var-ref,
                 :var-name "name"}
                {:start 62,
                 :end 73,
                 :groups ["{{/actors}}" "/actors"],
                 :type :section-close,
                 :section-name "actors"}]
               {:start 75,
                :end 85,
                :groups ["{{/movie}}" "/movie"],
                :type :section-close,
                :section-name "movie"}]
              {:start 88,
               :end 101,
               :groups ["{{something}}" "something"],
               :type :var-ref,
               :var-name "something"}])))))

(deftest feature-validation
  (testing "False values or Empty lists"
    (let [template "Shown.
{{#person}}
Never shown!
{{/person}}"]
      (is (= (#'sut/render template {:person false})
             "Shown.\n"))

      (is (= (#'sut/render template {})
             "Shown.\n"))))

  (testing "Non-Empty Lists"
    (is (= (#'sut/render "{{#repo}}
<b>{{name}}</b>
{{/repo}}
"
                         {:repo [{:name "resque"}
                                 {:name "hub"}
                                 {:name "rip"}]})

           "<b>resque</b>\n<b>hub</b>\n<b>rip</b>\n"))

    (is (= (#'sut/render "# Movies list
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
    (= (#'sut/render "{{#person?}}
Hi {{name}}!
{{/person?}}
"
                     {:person? {:name "Jon"}})

       "Hi Jon!\n"))

  (testing "Inverted Sections"
    (is (=
         (#'sut/render "{{#repo}}
<b>{{name}}</b>
{{/repo}}
{{^repo}}
No repos :(
{{/repo}}
"
                       {:repo []})

         "No repos :(\n")))

  (testing "Comments"
    (is (= (#'sut/render "<h1>Today{{! ignore me }}.</h1>" {})
           "<h1>Today.</h1>")))


  (testing "Set Delimiter"
    (is (= (#'sut/render "* {{hello}}
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

             [{:start 0,
               :end 12,
               :groups ["{{#   repo}}" "#   repo"],
               :type :section-open,
               :section-name "repo"}
              {:start 16,
               :end 24,
               :groups ["{{name}}" "name"],
               :type :var-ref,
               :var-name "name"}
              {:start 29,
               :end 41,
               :groups ["{{ /  repo}}" " /  repo"],
               :type :section-close,
               :section-name "repo"}])))))
