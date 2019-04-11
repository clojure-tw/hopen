(ns hopen.syntax.handlebars
  (:require [clojure.set :refer [rename-keys]]
            [clojure.string :as str]
            [clojure.zip :as z]
            [hopen.syntax.util :refer [re-quote]]
            [hopen.util :refer [throw-exception triml]]
            [instaparse.core #?@(:clj [:refer [defparser]]
                                 :cljs [:refer-macros [defparser]])]
            [instaparse.gll :refer [text->segment sub-sequence]]))

(def default-delimiters {:open "{{", :close "}}"})

(def ^:private close-delim-not-found-msg
  (str "The end of the template has been reached, "
       "but the closing delimiter was not found!"))

(defn- parse-change-delim
  "Parses a change-delimiter tag, returns nil if it was not one,
  returns `[matched-text open-delim close-delim]` otherwise."
  [segment close-delim]
  (let [re (re-pattern (str "^"
                            (re-quote "=")
                            "\\s+(\\S+)\\s+(\\S+)\\s+"
                            (re-quote "=")
                            (re-quote close-delim)))]
    (re-find re segment)))

(defn- parse-text-segments
  "Parses and collects all the text segments until a syntax block is found.

  This function handles and interprets the tags which are changing the
  delimiters, so that the rest of the program doesn't have to deal with it.

  Returns `[text-segments next-segment open-delim close-delim]`."
  [segment open-delim close-delim]
  (loop [text-segments []
         segment segment
         open-delim open-delim
         close-delim close-delim]
    (if-let [index (str/index-of segment open-delim)]
      (let [text-segments (conj text-segments (sub-sequence segment 0 index))
            syntax-segment (sub-sequence segment (+ index (count open-delim)))]
        ;; Is it a change-delimiter tag?
        (if-let [change-delimiters (parse-change-delim syntax-segment close-delim)]
          ;; Yes it is, so we should continue to parse more text segments.
          (let [[matched-text open-delim close-delim] change-delimiters]
            (recur text-segments
                   (sub-sequence syntax-segment (count matched-text))
                   open-delim
                   close-delim))
          ;; No it's a syntax segment, so we are done parsing text segments.
          [text-segments syntax-segment open-delim close-delim]))
      ;; The whole remaining text is the text segment, we are done.
      [(conj text-segments segment)
       nil
       open-delim
       close-delim])))

(defn- parse-syntax-segment
  "Parses the syntax segment, assuming that this function is provided a segment
  at the start of that syntax segment.

  Returns `[syntax-segment next-segment]`."
  [segment close-delim]
  (if-let [index (str/index-of segment close-delim)]
    [(sub-sequence segment 0 index)
     (sub-sequence segment (+ index (count close-delim)))]
    (throw-exception close-delim-not-found-msg)))

(defn- template-partition [delimiters]
  (fn [rf]
    (fn
      ([] (rf))
      ([result] (rf result))
      ([result template]
       (loop [result result
              segment (text->segment template)
              open-delim (:open delimiters)
              close-delim (:close delimiters)]
         ;; Read the text segments.
         (let [[text-segments next-segment open-delim close-delim]
               (parse-text-segments segment open-delim close-delim)

               result (cond-> result
                        (seq text-segments) (rf [:text text-segments]))]
           (if next-segment
             ;; Read the syntax segment.
             (let [[syntax-segment next-segment]
                   (parse-syntax-segment next-segment close-delim)]
               (recur (rf result [:syntax syntax-segment])
                      next-segment
                      open-delim
                      close-delim))
             ;; No more segments to read.
             result)))))))

(defn- handlebars-comment? [[type segment]]
  (and (= type :syntax)
       (or (re-matches #"\!\s+([\s\S]*)\s" segment)
           (re-matches #"\!\-\-\s+([\s\S]*)\s\-\-" segment))))

;; TODO: support line characters removal.
(def ^:private remove-killed-line-parts
  (fn [rf]
    (fn
      ([] (rf))
      ([result] (rf result))
      ([result input]
       (rf result input)))))

;; Regroups together the text segments,
;; removes empty texts,
;; removes empty text segments.
(def ^:private cleanup-text-segments
  (comp (partition-by first)
        (mapcat (fn [[[type] :as coll]]
                  (if (= type :text)
                    (let [segments (into []
                                         (comp (mapcat second)
                                               (remove empty?))
                                         coll)]
                      (when (seq segments)
                        [[:text segments]]))
                    coll)))))

(defparser handlebars-syntax-parser
  "syntax = (partial | open-block | else | else-if | close-block | <maybe-space> root-expression) <maybe-space>
   partial = <'>'> <space> symbol hash-params?
   open-block = <'#'> #'\\S+' ((<space> expression)* | each-as-args)
   each-as-args = <space> expression <space> <'as'>
                  <space> <'|'> <maybe-space> symbol <space> symbol <maybe-space> <'|'>
   else = <'else'>
   else-if = <'else'> <space> <'if'> <space> expression
   close-block = <'/'> symbol
   <root-expression> = value | dotted-term | fn-call

   fn-call = !keyword symbol (<space> expression)+ hash-params?
   hash-params = (<space> symbol <'='> expression)+
   <expression> = value | dotted-term | <'('> <maybe-space> fn-call <maybe-space> <')'>
   dotted-term = !keyword symbol (<'.'> symbol)*
   keyword = else | boolean-value
   <symbol> = #'[a-zA-Z_][a-zA-Z0-9_]*'
   <value> = string-value | boolean-value | number-value
   string-value = <'\"'> #'[^\"]*' <'\"'>
   boolean-value = 'true' | 'false'
   number-value = #'\\-?[0-9]+'
   space = #'\\s+'
   maybe-space = #'\\s*'"
  :output-format :enlive)

(defn- handlebars-node
  "Returns a handlebars node from an element of the segment partition."
  [[type segment]]
  (case type
    :text {:tag :text, :content (list (apply str segment))}
    :syntax (-> (handlebars-syntax-parser (str segment)) :content first)))

(defn- handlebars-zipper
  ([] (handlebars-zipper {:tag :root}))
  ([root] (z/zipper (comp #{:root :open-block} :tag)                           ; branch?
                    :children
                    (fn [node children] (assoc node :children (vec children))) ; make-node
                    root)))

(defn- children->then [node]
  (assert (not (:then node)) "There are multiple `else` for the same `if`.")
  (rename-keys node {:children :then}))

(defn- find-opening-block [zipper closing-node]
  (let [closing-block-name (-> closing-node :content first)]
    (some (fn [z]
            (assert (some? z) "No opening block found.")
            (let [node (z/node z)]
              (when (and (= (:tag node) :open-block)
                         (not (:did-not-open-a-block node)))
                (assert (= (-> node :content first) closing-block-name)
                        "The closing block does not match the opening block.")
                z)))
          (iterate z/up zipper))))

(defn- handlebars-zipper-reducer
  "Builds a tree-shaped representation of the handlebar's nodes."
  ([] (handlebars-zipper))
  ([zipper] zipper)
  ([zipper node]
   (case (:tag node)
     :open-block  (-> zipper
                      (z/append-child node)
                      (z/down)
                      (z/rightmost))
     :else        (-> zipper
                      (z/edit children->then))
     :else-if     (-> zipper
                      (z/edit children->then)
                      (z/append-child (-> node
                                          (assoc :tag :open-block
                                                 :did-not-open-a-block true)
                                          (update :content conj "if")))
                      (z/down))
     :close-block (-> zipper
                      (find-opening-block node)
                      z/up)
     (z/append-child zipper node))))

;; TODO: support the `..`
(defn- to-data-template
  "Generates a data-template from a handlebars tree's node."
  [node]
  (let [{:keys [tag content children]} node
        [arg0] content]
    (case tag
      :root (mapv to-data-template children)
      (:text :string-value) arg0
      (:boolean-value :number-value) (read-string arg0)
      :fn-call (let [[func & args] content]
                 (list* (symbol func) (map to-data-template args)))
      :dotted-term (if (= (count content) 1)
                     (list 'hopen/ctx (keyword arg0))
                     (list 'get-in 'hopen/ctx (mapv keyword content)))
      :hash-params (into {}
                         (comp (partition-all 2)
                               (map (fn [[k v]] [(keyword k) (to-data-template v)])))
                         content)
      :open-block
      (let [[block-name arg0] content]
        (case block-name
          "if"   (if-let [then (seq (:then node))]
                   (list 'b/if (to-data-template arg0)
                         (mapv to-data-template then)
                         (mapv to-data-template children))
                   (list 'b/if (to-data-template arg0)
                         (mapv to-data-template children)))
          "with" (list 'b/let ['hopen/ctx (to-data-template arg0)]
                       (mapv to-data-template children))
          "each" (let [for-binding (if (= (:tag arg0) :each-as-args)
                                     (let [[coll var index] (:content arg0)]
                                       [(symbol var) (to-data-template coll)
                                        :indexed-by (symbol index)])
                                     ['hopen/ctx (to-data-template arg0)])]
                   (list 'b/for for-binding
                         (mapv to-data-template children)))))
      ["Unhandled:" node])))

(defn parse [template]
  (-> (transduce (comp (template-partition default-delimiters)
                       (remove handlebars-comment?)
                       cleanup-text-segments
                       (map handlebars-node))
                 handlebars-zipper-reducer
                 [template])
      z/root
      to-data-template))
