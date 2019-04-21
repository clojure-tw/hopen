(ns hopen.syntax.handlebars-test
  (:require #?(:clj  [clojure.test :refer [deftest testing is are]]
               :cljs [cljs.test    :refer [deftest testing is are]
                                   :include-macros true])
            [hopen.util :refer [triml]]
            [hopen.syntax.handlebars :as hb :refer [parse with-handlebars-env]]
            [hopen.renderer.env :refer [standard-env]]
            [hopen.renderer.xf :refer [renderer with-renderer-env]]))

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
       '(hopen/root :title)
       (triml "</h1>
             |  <h2>By ")
       '(get-in hopen/root [:author :name])
       (triml "</h2>
             |
             |  <div class=\"body\">
             |    ")
       '(hopen/root :body)
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
      '["aa  bb " (get-in hopen/root [:cc :dd :ee]) " ff"]

      "{{false}}{{true}}"
      [false true]

      "{{0}}{{3}}{{-0}}{{-3}}"
      [0 3 -0 -3]

      "{{ \"some text\" }}"
      ["some text"]

      "{{foo bar a.b}}"
      '[(foo (hopen/root :bar) (get-in hopen/root [:a :b]))]

      "{{foo bar (a b.c)}}"
      '[(foo (hopen/root :bar) (a (get-in hopen/root [:b :c])))]

      "{{foo bar a.b c=d e=true f=3 g=\"hello\"}}"
      '[(foo (hopen/root :bar)
             (get-in hopen/root [:a :b])
             {:c (hopen/root :d)
              :e true
              :f 3
              :g "hello"})]

      "a{{#if b}}c{{/if}}d"
      '["a"
        (b/if (hb/true? (hopen/root :b))
          ["c"])
        "d"]

      "a{{#if b}}c{{else}}d{{/if}}e"
      '["a"
        (b/if (hb/true? (hopen/root :b))
          ["c"]
          ["d"])
        "e"]

      "a{{#if b}}c{{else if d}}e{{/if}}f"
      '["a"
        (b/if (hb/true? (hopen/root :b))
          ["c"]
          [(b/if (hb/true? (hopen/root :d))
             ["e"])])
        "f"]

      "a{{#if b}}c{{else if d}}e{{else}}f{{/if}}g"
      '["a"
        (b/if (hb/true? (hopen/root :b))
          ["c"]
          [(b/if (hb/true? (hopen/root :d))
             ["e"]
             ["f"])])
        "g"]

      "a{{#unless b}}c{{/unless}}d"
      '["a"
        (b/if (hb/false? (hopen/root :b))
          ["c"])
        "d"]

      "{{#each a.b}}c{{/each}}"
      '[(b/for [hb/pair (hb/as-kvs (get-in hopen/root [:a :b]))]
          [(b/let [hb/ctx1 (second hb/pair)]
             ["c"])])]

      "{{#each a.b}}{{@index}}c{{/each}}"
      '[(b/for [hb/pair (hb/as-kvs (get-in hopen/root [:a :b])) :indexed-by hb/index1]
          [(b/let [hb/ctx1 (second hb/pair)]
             [hb/index1
              "c"])])]

      "{{#each a.b}}{{@key}}c{{/each}}"
      '[(b/for [hb/pair (hb/as-kvs (get-in hopen/root [:a :b]))]
          [(b/let [hb/ctx1 (second hb/pair)
                   hb/key1 (first hb/pair)]
             [hb/key1
              "c"])])]

      "{{#with a}}b{{/with}}"
      '[(b/let [hb/ctx1 (hopen/root :a)]
          ["b"])]

      "{{#with a.b}}c{{/with}}"
      '[(b/let [hb/ctx1 (get-in hopen/root [:a :b])]
          ["c"])]

      "aa {{#if bb}} cc {{#each dd.dd}} ee {{/each}} ff {{/if}} gg"
      '["aa "
        (b/if (hb/true? (hopen/root :bb))
          [" cc "
           (b/for [hb/pair (hb/as-kvs (get-in hopen/root [:dd :dd]))]
             [(b/let [hb/ctx1 (second hb/pair)]
                [" ee "])])
           " ff "])
        " gg"]

      "{{#each coll as |x i|}}d{{/each}}"
      '[(b/for [hb/pair (hb/as-kvs (hopen/root :coll))]
          [(b/let [hb/ctx1 (assoc hopen/root
                                  :i (first hb/pair)
                                  :x (second hb/pair))]
             ["d"])])]

      "{{#each coll as |x i|}}{{@index}}d{{/each}}"
      '[(b/for [hb/pair (hb/as-kvs (hopen/root :coll)) :indexed-by hb/index1]
          [(b/let [hb/ctx1 (assoc hopen/root
                                  :i (first hb/pair)
                                  :x (second hb/pair))]
             [hb/index1
              "d"])])]

      "{{#each coll as |x i|}}{{@key}}d{{/each}}"
      '[(b/for [hb/pair (hb/as-kvs (hopen/root :coll))]
          [(b/let [hb/ctx1 (assoc hopen/root
                                  :i (first hb/pair)
                                  :x (second hb/pair))
                   hb/key1 (first hb/pair)]
             [hb/key1
              "d"])])]

      "a {{> confirm-button}} b"
      '["a "
        (b/template :confirm-button hopen/root)
        " b"]

      "a {{> confirm-button title=\"Alright\"}} b"
      '["a "
        (b/template :confirm-button (merge hopen/root {:title "Alright"}))
        " b"])))

(deftest handlebars-false?-test
  (testing "truthy things"
    (are [val]
      (not (#'hb/handlebars-false? val))

      true
      3
      "hi"
      [""]
      {"" ""}
      #{""}))

  (testing "falsey things"
    (are [val]
      (#'hb/handlebars-false? val)

      nil
      false
      0
      ""
      []
      {}
      #{})))

(deftest handlebars-renderer-integration-test
  (let [env (-> standard-env
                (with-handlebars-env)
                (with-renderer-env))]
    (are [hb-template data expected-result]
      (= (into [] (renderer (parse hb-template) env) [data])
         expected-result)

      "{{#with a}}{{b}}{{c.d}}e{{/with}}"
      {:a {:b 1, :c {:d 2}}}
      [1 2 "e"]

      "{{#each coll as |x i|}}{{x}}{{i}}d{{/each}}"
      {:coll [:a :b :c]}
      [:a 0 "d" :b 1 "d" :c 2 "d"]

      "{{#each coll as |x i|}}{{x}}{{i}}d{{/each}}"
      {:coll {:a "aa" :b "bb"}}
      ["aa" :a "d" "bb" :b "d"])))
