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
            {:start 37, :end 57, :groups ["<% erb_style_tags %>" " erb_style_tags "]}
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
"
          ]
      (is (= (->> text
                  (#'sut/retrieve-all-tags)
                  (#'sut/tags->ast))

             [{:start 2, :end 15, :groups ["{{something}}" "something"]}
              [{:start 18, :end 29, :groups ["{{#movies}}" "#movies"]}
               [{:start 33, :end 44, :groups ["{{#actors}}" "#actors"]}
                {:start 50, :end 58, :groups ["{{name}}" "name"]}
                {:start 62, :end 73, :groups ["{{/actors}}" "/actors"]}]
               {:start 75, :end 85, :groups ["{{/movie}}" "/movie"]}]
              {:start 88, :end 101, :groups ["{{something}}" "something"]}]))))

  )
