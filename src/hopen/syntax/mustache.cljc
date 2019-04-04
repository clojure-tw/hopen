(ns hopen.syntax.mustache
  #?(:clj (:import [java.util.regex Pattern]))
  (:require [clojure.string :as str]
            [hopen.syntax.util :as util]
            [clojure.zip :as zip]
            [clojure.walk :as walk]
            [backtick]))

;;------------------------------------------------------------------------------
;; General utilities
;;------------------------------------------------------------------------------

;; TODO!!! Provide a js implementation
(defn re-seq-pos
  "Returns a lazy sequence of regex matches like `re-seq`, but also returns the offsets of
  the groups/captures."
  [pattern string]
  (let [m (re-matcher pattern string)]
    ((fn step []
       (when (. m find)
         (cons {:start (.start m)
                :end (.end m)
                :groups (into [] (for [g (range (inc (.groupCount m)))]
                                   (.group m g)))}
               (lazy-seq (step))))))))

(defn conj-last
  "Conj `x` into the last element of the `coll`.
  `coll` should be a seq of seqs."
  [coll x]
  (conj (pop coll) (conj (peek coll) x)))

(defn conj-some
  [coll & xs]
  (->> xs
       (filter some?)
       (apply conj coll)))

;;------------------------------------------------------------------------------
;; Namespace specific functions
;;------------------------------------------------------------------------------

(defn- compile-regex [start-delim end-delim]
  (re-pattern
   (str
    (util/re-quote start-delim)
    "(.*)"
    (util/re-quote end-delim))))

(defn- parse-delim-change-tag
  "Try to parse the tag-text as a delimiter change command.
  Returns '(start-delim end-delim) or nil."
  [tag-text]
  (let [result (re-matches #"=(.*)\s+(.*)=" tag-text)]
    (when (= 3 (count result))
      (drop 1 result))))

(defn- tag--text [tag]
  (get-in tag [:groups 1]))

(defn- retrieve-all-tags
  "Retrieve a list of all tags and their offsets.
  Any delimiter changes will be dealt with here.

  A tag is represented by a single entry returned by `re-seq-pos`,
  which looks like:
  {:start 'character offset into text where the tag starts'
   :end 'character offset into text where the tag ends'

   :groups ['full text of tag with delimiters' 'text of tag without delimiters']}"
  [text]

  (let [update-tags-with-offset (fn [offset tags]
                                  (if (zero? offset)
                                    tags
                                    (map #(-> %
                                              (update :start + offset)
                                              (update :end + offset))
                                         tags)))]
    (loop [regex (compile-regex "{{" "}}")
           offset 0
           tags []]

      (let [;; Given the tags we can find with our current delimiter.
            ;; Collect all tags that do not denote delim change.
            ;; When we do find a delim change, stop further processing.
            possible-tags (->> (re-seq-pos regex (subs text offset))
                               (reduce (fn [accum tag]
                                         (let [delims (parse-delim-change-tag (tag--text tag))]
                                           (if delims
                                             (reduced (conj accum (assoc tag :delims delims)))
                                             (conj accum tag))))
                                       [])
                               (update-tags-with-offset offset))]

        ;; If we did not encounter a delim change tag...
        (if-not (->> possible-tags last :delims)
          ;; We're done retrieving all tags
          (into tags possible-tags)

          ;; We did encounter a delim change...
          ;; Continue processing the rest of the text with the new delimiter
          (recur (apply compile-regex (->> possible-tags last :delims))

                 ;; restart at the end of the change delim tag
                 (->> possible-tags last :end)

                 (into tags (drop-last possible-tags))))))))

;; TODO!!! Better tag content analysis
(defn- assoc-tag-type [tag]
  (let [text (tag--text tag)]
    (cond
      (= \# (first text)) (assoc tag :type :section-open)
      (= \/ (first text)) (assoc tag :type :section-close)
      (= \^ (first text)) (assoc tag :type :inverted-open)
      (= \! (first text)) (assoc tag :type :comment)
      (= \> (first text)) (assoc tag :type :partial)
      :else (assoc tag :type :var-ref))))

(defn- discard-first-newline [s]
  (cond
    (str/starts-with? s "\n")
    (subs s 1)
    (str/starts-with? s "\r\n")
    (subs s 2)))

(defn fill-in-text
  "Takes a linear list of tags and fills in the text that are in between the tags"
  [text tags]
  (let [text-between (fn [last-tag this-tag text]
                       (let [result (cond->> (if this-tag
                                               (subs text (:end last-tag) (:start this-tag))
                                               (subs text (:end last-tag)))

                                      ;; Section open and close tags should not cause additional newlines to be inserted
                                      (contains? #{:section-open :section-close :inverted-open} (:type last-tag))
                                      (discard-first-newline))]
                         (if (empty? result)
                           nil
                           result)))

        result (reduce (fn [accum tag]
                         (if (empty? accum)
                           (if (zero? (:start tag))
                             (conj accum tag)
                             (conj accum
                                   (subs text 0 (:start tag))
                                   tag))

                           (conj-some accum
                                      (text-between (last accum) tag text)
                                      tag)))
                       []
                       tags)]

    (if (> (count text) (:end (last tags)))
      (conj-some result (text-between (last tags) nil text))
      result)))

;; TODO!!! Should throw errors when the blocks aren't matched
(defn tags->ast
  "Given a flat list of `tags`, return another list of `tags` but with blocks properly grouped.

  Specifically, tags with content that start with the characters '#' or '^' opens a block.
  Tag with '/' closes a block."
  [tags]
  (->> tags
       (reduce (fn [stack tag]
                 (if (string? tag)
                   (conj-last stack tag)

                   (let [tag-text (str/trim (tag--text tag))]
                     (cond
                       ;; If we find something that opens a block,
                       ;; push a new context on the stack
                       (contains? #{\# \^} (first tag-text))
                       (conj stack [tag])

                       ;; If we find something that closes a block...
                       (= \/ (first tag-text))
                       (as-> (peek stack) $
                         (conj $ tag)               ;; add closing tag to last context
                         (conj-last (pop stack) $)) ;; move last context into parent context

                       ;; Add the tag to the active context
                       :else
                       (conj-last stack tag)))))
               [[]])  ;; start with stack with a single empty top level context
       first))

(defn- clean-var-ref [text]
  (str/trim text))

(defn ast-node->hopen-node
  [node]

  (cond
    (string? node) node
    (map? node) (cond
                  (= :section-open (:type node)) node
                  (= :var-ref (:type node))
                  (list 'ctx-lookup (keyword (clean-var-ref (tag--text node))))
                  (= :section-close (:type node)) node
                  (= :inverted-open (:type node)) node
                  (= :comment (:type node)) nil
                  (= :partial (:type node)) nil)

    (vector? node)
    (cond
      (= :section-open (:type (first node))) (let [contents (->> node
                                                                 (drop 1)
                                                                 (drop-last 1)
                                                                 (into []))]
                                               (backtick/template
                                                (b/let [data (ctx-lookup ~(keyword (subs (tag--text (first node)) 1)))]
                                                  [(b/if (and data
                                                              (not (empty? data)))
                                                     [(b/if (or (list? data)
                                                                (vector? data))

                                                        ;; Looks like the context might have multiple items
                                                        [(b/for [ctx data]
                                                           [(b/let [hopen/ctx (conj hopen/ctx ctx)]
                                                              ~contents)])]

                                                        ;; New context only has a single item
                                                        [(b/let [hopen/ctx (conj hopen/ctx data)]
                                                           ~contents)])])])))

      (= :inverted-open (:type (first node))) (backtick/template
                                               (b/let [data (ctx-lookup ~(keyword (subs (tag--text (first node)) 1)))]
                                                 [(b/if (or (= data nil)
                                                            (empty? data))
                                                    ~(->> node
                                                          (drop 1)
                                                          (drop-last 1)
                                                          (into [])))]))
      )
      ))

(defn postwalk-zipper [f loc opts]
  (let [;; Locate a single node to modify
        loc (if-some [;; try to step into the first child
                      loc (zip/down loc)]

              ;; For each child in this branch...
              (loop [loc loc]

                ;; Recursively visit & map/mutate the child node.
                (let [loc (postwalk-zipper f loc opts)]

                  ;; If there are more child nodes, keep processing...
                  (if-some [loc (zip/right loc)]
                    (recur loc)

                    ;; All children have been processed first...
                    ;; Now process this node itself
                    (zip/up loc))))

              ;; No child nodes?
              ;; We've reached a leaf node
              loc)]

    ;; map/mutate the node
    (if (and (:skip-root opts)
             (not (some? (zip/up loc))))
      loc
      (zip/replace loc (f (zip/node loc))))))

(defn parse [text]
  (as-> text $
    (retrieve-all-tags $)
    (map assoc-tag-type $)  ;; Analyze and attach tag types
    (fill-in-text text $)   ;; Add missing text back into the tag sequence
    (tags->ast $)           ;; Nest structures/sections/blocks as needed

    ;; Convert result to data hopen can execute on
    (postwalk-zipper ast-node->hopen-node (zip/vector-zip $) {:skip-root true})
    (zip/root $)))


(comment

  (parse "Hello, {{name}}")


  (parse
"{{#movies}}
# {{name}}
{{/movies}}
")


  (parse
"# Movies list
{{#movies}}
# {{name}}
## Actors
{{#cast}}
* {{name}}
{{/cast}}
{{/movies}}
")


  (let [data-template (parse
"# Movies list
{{#movies}}
## {{name}}
{{/movies}}
")]
    (into [] (hopen.renderer.xf/renderer data-template)
          [[{:movies [{:name "Tropic Thunder"
                       :cast [{:name "Ben Stiller"}
                              {:name "RDJ"}
                              {:name "Jack Black"}]}
                      {:name "The secret life of Walter Mitty"
                       :cast [{:name "Ben Stiller"}
                              {:name "Kristen Wiig"}]}]}]]))


   (let [data-template (parse
"# Movies list
{{#movies}}
## {{name}}
### Actors
{{#cast}}
* {{name}}
{{/cast}}
{{/movies}}
")]
     (->> (into [] (hopen.renderer.xf/renderer data-template)
                [[{:movies [{:name "Tropic Thunder"
                             :cast [{:name "Ben Stiller"}
                                    {:name "RDJ"}
                                    {:name "Jack Black"}]}
                            {:name "The secret life of Walter Mitty"
                             :cast [{:name "Ben Stiller"}
                                    {:name "Kristen Wiig"}]}]}]])
          (apply str)
          (println)))

   (let [data-template (parse
"# Movies list
{{#movies}}
## {{name}}
### Actors
{{#cast}}
* {{name}}
{{/cast}}
{{/movies}}
{{^movies}}
None found
{{/movies}}
")]
     (->> (into [] (hopen.renderer.xf/renderer data-template)
                [[{}]])
          (apply str)))

   (let [data-template (parse
"{{#person?}}
Hi {{name}}!
{{/person?}}
")]
     (->> (into [] (hopen.renderer.xf/renderer data-template)
                [[{:person? {:name "Jon"}}]])
          (apply str)))

  )
