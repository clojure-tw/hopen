(ns hopen.syntax.mustache
  (:require [clojure.string :as str]
            [hopen.syntax.util :as util]
            [clojure.zip :as zip]
            [clojure.walk :as walk]
            [backtick.core :as backtick]
            [better-cond.core :as b]))

;;------------------------------------------------------------------------------
;; General utilities
;;------------------------------------------------------------------------------

(defn re-seq-pos
  "Returns a lazy sequence of regex matches like `re-seq`, but also returns the offsets of
  the groups/captures."
  [pattern string]
  #?(:clj
     (let [m (re-matcher pattern string)]
       ((fn step []
          (when (. m find)
            (cons {:start (.start m)
                   :end (.end m)
                   :groups (into [] (for [g (range (inc (.groupCount m)))]
                                      (.group m g)))}
                  (lazy-seq (step)))))))

     :cljs
     ;; It'd be really nice if we can just use the global flag for the js regex
     ;; However, https://dev.clojure.org/jira/browse/CLJS-150 (from 2014) suggets that
     ;; it makes use of some global state, which may mess up the result.
     ;;
     ;; This is an altered version of cljs' re-seq.
     (letfn [(re-seq-pos* [re s current-offset]
               (when-some [matches (.exec re s)]
                 (let [match-str (aget matches 0)
                       start (+ current-offset (.-index matches))
                       end (+ start (count match-str))]
                   (cons {:start start
                          :end end
                          :groups (if (== (.-length matches) 1)
                                    match-str
                                    (vec matches))}
                         (lazy-seq
                          (let [post-idx (+ (.-index matches)
                                            (max 1 (.-length match-str)))]
                            (when (<= post-idx (.-length s))
                              (re-seq-pos* re (subs s post-idx) (+ current-offset post-idx)))))))))]
     (if (string? string)
         (re-seq-pos* pattern string 0)
         (throw (js/TypeError. "re-seq-pos must match against a string."))))))

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
  (if (and (= start-delim "{{")
           (= end-delim "}}"))
    #"\{\{\{(.*?)\}\}\}|\{\{(.*?)\}\}"
    (re-pattern
     (str
      (util/re-quote start-delim)
      "\\{(.*?)\\}"
      (util/re-quote start-delim)
      "|"
      (util/re-quote start-delim)
      "(.*?)"
      (util/re-quote end-delim)))))

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

                               ;; Extract the captured contents of the tag
                               (map (fn [tag]
                                      (if (get-in tag [:groups 2])
                                        (assoc tag :content (get-in tag [:groups 2]))
                                        (assoc tag
                                               :content (get-in tag [:groups 1])
                                               :skip-escape true))))

                               ;; Tag delim changes with a specific type tag
                               ;; We'll use this later to decide if we need continue processing
                               ;; with another set of delimiters.
                               (reduce (fn [accum tag]
                                         (let [delims (parse-delim-change-tag (:content tag))]
                                           (if delims
                                             (reduced (conj accum (assoc tag
                                                                         :delims delims
                                                                         :type :delim-change)))
                                             (conj accum tag))))
                                       [])

                               ;; Account for the current offset
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

(defn- retrieve-newline-offsets [text]
  (let [offsets (->> (re-seq-pos #"(\r\n|\r|\n)" text)
                     (map :start)
                     (into []))]

    ;; If text doesn't end with a newline,
    ;; add a fake newline at the end of the string so every line in the text has
    ;; an associated line end offset
    (if (not= (last offsets) (dec (count text)) )
      (conj offsets (count text))
      offsets)))

(defn- join-tag-lineno [tags text]
  (letfn [(update-tag-with-current-lineno [state tag]
            (update state :tags conj (assoc tag :lineno (:lineno state))))]

    (->> tags
         (reduce (fn [state tag]
                   ;; Is the tag outside the current line?
                   (if (> (:start tag) (first (:newlines state)))
                     ;; Update :newlines & lineno until we reach the line this tag is on
                     (-> (reduce (fn [state newline-offset]
                                   ;; Consume some newlines while tracking where we are

                                   (if (> (:start tag) newline-offset)
                                     (-> state
                                         (update :lineno inc)
                                         (update :newlines rest))
                                     (reduced state)))
                                 state
                                 (:newlines state))

                         (update-tag-with-current-lineno tag))

                     ;; Add the current number to the tag
                     (update-tag-with-current-lineno state tag)))
                 {:newlines (retrieve-newline-offsets text)
                  :lineno 1
                  :tags []})
         :tags)))

(defn- assoc-tag-type [tag]
  (if (:type tag)
    tag  ;; don't alter tags with types already assigned

    ;; No type has been assigned yet...
    (let [text (:content tag)]
      (b/cond
        ;; Does the text look like some kind of section tag??
        :let [parsed-tag (re-matches #"^\s*([\#\/\^\>])\s*([a-zA-Z0-9\?-]+)" text)]
        (some? parsed-tag) (-> (cond
                                  (= "#" (second parsed-tag)) (assoc tag :type :section-open)
                                  (= "/" (second parsed-tag)) (assoc tag :type :section-close)
                                  (= "^" (second parsed-tag)) (assoc tag :type :inverted-open)
                                  (= ">" (second parsed-tag)) (assoc tag :type :partial)
                                  :else (throw (ex-info (str "Unhandled section tag encountered: " text) {})))
                               (assoc :section-name (nth parsed-tag 2)))

        ;; Does it look like a comment?
        ;; A comment is anything that starts with a "!"
        (some? (re-find #"^\s*\!" text)) (assoc tag :type :comment)

        ;; Does the text look like some kind of variable name?
        :let [var-name-match (re-matches #"^\s*(&?)\s*([a-zA-Z0-9\?-]+)\s*" text)]
        (some? var-name-match) (cond-> tag
                                 (not (empty? (second var-name-match)))
                                 (assoc :skip-escape (some? (second var-name-match)))

                                 :then
                                 (assoc :type :var-ref
                                        :var-name (nth var-name-match 2)))

        ;; We don't really know what we're looking at.
        ;; Report something has gone wrong.
        :else
        (throw (ex-info (str "Invalid tag: " text) {}))))))

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

(defn tags->ast
  "Given a flat list of `tags`, return another list of `tags` but with blocks properly grouped.

  Specifically, tags with content that start with the characters '#' or '^' opens a block.
  Tag with '/' closes a block."
  [tags]

  (let [result (reduce (fn [stack tag]
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
                       [[]]  ;; start with stack with a single empty top level context
                       tags)]

    ;; We've processed all the tags.
    ;; If all went well, we should have exactly one open contet that represents a valid nested sections.
    ;; If the user left some sections unclosed, we'd end up with multiple contexts here.
    ;; We'll place these into the root context for now.
    ;; This error will be detected and reported in `ast-sanity-check`.
    (if (< 1 (count result))
      (apply conj (first result) (rest result))
      (first result))))

(defn ast-sanity-check [ast]
  (doseq [node (tree-seq vector? seq ast)]
    (when (map? (first node))
      (let [open-tag (first node)
            close-tag (last node)]
        (if (not= (:section-name open-tag) (:section-name close-tag))
          (throw (ex-info (str "Section tag \"" (get-in open-tag [:groups 0])
                               "\" opened on line " (:lineno open-tag)
                               " is never closed") {}))))))
  ast)

(defn ast-node->hopen-node
  [node]

  (cond
    (string? node) node
    (map? node) (cond
                  (= :section-open (:type node)) node
                  (= :var-ref (:type node))
                  (if (:skip-escape node)
                    (backtick/template
                     (ctx-lookup ~(keyword (:var-name node))))
                    (backtick/template
                     (escape-html (ctx-lookup ~(keyword (:var-name node))))))
                  (= :section-close (:type node)) node
                  (= :inverted-open (:type node)) node
                  (= :comment (:type node)) nil
                  (= :partial (:type node)) nil
                  (= :delim-change (:type node)) nil
                  :else (throw (ex-info (str "Does not how how to deal with tags of type: " (:type node)) {})))

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
    (map assoc-tag-type $)   ;; Analyze and attach tag types
    (join-tag-lineno $ text) ;; Add line number info to all tags
    (fill-in-text text $)    ;; Add missing text back into the tag sequence
    (tags->ast $)            ;; Nest structures/sections/blocks as needed
    (ast-sanity-check $)     ;; Do some basic error checking on the ast

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
