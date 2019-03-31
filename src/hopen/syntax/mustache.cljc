(ns hopen.syntax.mustache
  #?(:clj (:import [java.util.regex Pattern]))
  (:require [clojure.string :as str]))

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


;;------------------------------------------------------------------------------
;; Namespace specific functions
;;------------------------------------------------------------------------------

;; TODO!!! Provide a js implementation
(defn- compile-regex [start-delim end-delim]
  (Pattern/compile
   (str
    (Pattern/quote start-delim)
    "(.*)"
    (Pattern/quote end-delim))))

(defn- parse-delim-change-tag
  "Try to parse the tag-text as a delimiter change command.
  Returns '(start-delim end-delim) or nil."
  [tag-text]
  (let [result (re-matches #"=(.*)\s+(.*)=" tag-text)]
    (when (= 3 (count result))
      (drop 1 result))))

(defn- retrieve-all-tags
  "Retrieve a list of all tags and their offsets.
  Any delimiter changes will be dealt with here.

  A tag is represented by a single entry returned by `re-seq-pos`,
  which looks like:
  {:start 'character offset into text where the tag starts'
   :end 'character offset into text where the tag ends'

   :groups ['full text of tag with delimiters' 'text of tag without delimiters']}"
  [text]

  ;; A tag is represented by a single entry returned by `re-seq-pos`.
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
                                         (let [delims (parse-delim-change-tag (get-in tag [:groups 1]))]
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


;; TODO!!! Should throw errors when the blocks aren't matched
(defn tags->ast
  "Given a flat list of `tags`, return another list of `tags` but with blocks properly grouped.

  Specifically, tags with content that start with the characters '#' or '^' opens a block.
  Tag with '/' closes a block."
  [tags]
  (->> tags
       (reduce (fn [stack tag]
                 (let [tag-text (str/trim (get-in tag [:groups 1]))]
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
                     (conj-last stack tag))))
               [[]])  ;; start with stack with a single empty top level context
       first))

(defn parse [text]
  (->> text
       retrieve-all-tags
       tags->ast))
