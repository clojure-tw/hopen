(ns hopen.syntax.mustache
  #?(:clj (:import [java.util.regex Pattern]))
  (:require [clojure.string :as str]
            [hopen.syntax.util :as util]
            [clojure.zip :as zip]
            [clojure.walk :as walk]
            [backtick]
            [better-cond.core :as b]))

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

(defn deep-merge
  [a b]
  (if (map? a)
    (into a (for [[k v] b]
              [k (deep-merge (a k) v)]))
    b))

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
                                             (reduced (conj accum (assoc tag
                                                                         :delims delims
                                                                         :type :delim-change)))
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

                 (into tags possible-tags)))))))

(defn- assoc-tag-type [tag]
  (if (:type tag)
    tag  ;; don't alter tags with types already assigned

    ;; No type has been assigned yet...
    (let [text (tag--text tag)]
      (b/cond
        ;; Does the text look like some kind of section tag??
        :let [parsed-tag (re-matches #"^\s*([\#\/\^\>])\s*([a-zA-Z0-9\?-]+)" text)]
        (some? parsed-tag) (-> (cond
                                  (= "#" (second parsed-tag)) (assoc tag :type :section-open)
                                  (= "/" (second parsed-tag)) (assoc tag :type :section-close)
                                  (= "^" (second parsed-tag)) (assoc tag :type :inverted-open)
                                  (= ">" (second parsed-tag)) (assoc tag :type :partial)
                                  :else (throw (Exception. (str "Unhandled section tag encountered: " text))))
                               (assoc :section-name (nth parsed-tag 2)))

        ;; Does it look like a comment?
        ;; A comment is anything that starts with a "!"
        (some? (re-find #"^\s*\!" text)) (assoc tag :type :comment)

        ;; Does the text look like some kind of variable name?
        :let [var-name-match (re-matches #"^\s*([a-zA-Z0-9\?-]+)\s*" text)]
        (some? var-name-match) (assoc tag
                                      :type :var-ref
                                      :var-name (second var-name-match))

        ;; We don't really know what we're looking at.
        ;; Report something has gone wrong.
        :else
        (throw (Exception. (str "Invalid tag: " text)))))))

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
                                      (contains? #{:section-open :section-close :inverted-open :delim-change} (:type last-tag))
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

                   (cond
                     ;; If we find something that opens a block,
                     ;; push a new context on the stack
                     (contains? #{:section-open :inverted-open} (:type tag))
                     (conj stack [tag])

                     ;; If we find something that closes a block...
                     (= :section-close (:type tag))
                     (as-> (peek stack) $
                       (conj $ tag)               ;; add closing tag to last context
                       (conj-last (pop stack) $)) ;; move last context into parent context

                     ;; Add the tag to the active context
                     :else
                     (conj-last stack tag))))
               [[]])  ;; start with stack with a single empty top level context
       first))

(defn ast-node->hopen-node
  [node]

  (cond
    (string? node) node
    (map? node) (cond
                  (= :section-open (:type node)) node
                  (= :var-ref (:type node))
                  (list 'ctx-lookup (keyword (:var-name node)))
                  (= :section-close (:type node)) node
                  (= :inverted-open (:type node)) node
                  (= :comment (:type node)) nil
                  (= :partial (:type node)) nil
                  (= :delim-change (:type node)) nil
                  :else (throw (Exception. (str "Does not how how to deal with tags of type: " (:type node)))))

    (vector? node)
    (cond
      (= :section-open (:type (first node))) (let [contents (->> node
                                                                 (drop 1)
                                                                 (drop-last 1)
                                                                 (into []))]
                                               (backtick/template
                                                (b/let [data (ctx-lookup ~(keyword (:section-name (first node))))]
                                                  [(b/if (and data
                                                              (not (empty? data)))
                                                     [(b/if (or (list? data)
                                                                (vector? data))

                                                        ;; Looks like the context might have multiple items
                                                        [(b/for [ctx data]
                                                           [(b/let [mustache/ctx (conj mustache/ctx ctx)]
                                                              ~contents)])]

                                                        ;; New context only has a single item
                                                        [(b/let [mustache/ctx (conj mustache/ctx data)]
                                                           ~contents)])])])))

      (= :inverted-open (:type (first node))) (backtick/template
                                               (b/let [data (ctx-lookup ~(keyword (:section-name (first node))))]
                                                 [(b/if (or (= data nil)
                                                            (empty? data))
                                                    ~(->> node
                                                          (drop 1)
                                                          (drop-last 1)
                                                          (into [])))])))))

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


(defn- ctx-lookup
  "Tries to look up a field name in every context starting from most nested context first."
  [env field-name]
  (some #(get % (keyword field-name)) (reverse (get-in env [:bindings 'mustache/ctx] []))))

(defn init-env [env data-root]
  (deep-merge
   env
   {:inline-macro {'ctx-lookup ctx-lookup}
    :bindings {'mustache/ctx [data-root]}}))

(defn- render [template data]
  (let [data-template (parse template)]
    (->> (into [] (#'hopen.renderer.xf/renderer data-template (init-env hopen.renderer.xf/default-env data))
               [data])
         (apply str))))
