(ns hopen.syntax.handlebars
  (:require [clojure.set :refer [rename-keys]]
            [clojure.zip :as z]
            [hopen.syntax.partition :as part]
            [hopen.util :refer [parse-long]]
            [instaparse.core :as insta #?@(:clj  [:refer [defparser]]
                                           :cljs [:refer-macros [defparser]])]))

(defn- handlebars-comment? [[type segment]]
  (and (= type :syntax)
       (or (re-matches #"\!\s+([\s\S]*)" segment)
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
   <symbol> = #'[a-zA-Z_-][a-zA-Z0-9_-]*'
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
    :syntax (->> (handlebars-syntax-parser (str segment))
                 (insta/transform {:open-block (fn [block-type & content-rest]
                                                 {:tag :open-block
                                                  :block-type (keyword block-type)
                                                  :content content-rest})
                                   :close-block (fn [block-type & content-rest]
                                                  {:tag :close-block
                                                   :block-type (keyword block-type)
                                                   :content content-rest})})
                 :content
                 first)))

(defn- handlebars-zipper
  ([] (handlebars-zipper {:tag :root}))
  ([root] (z/zipper (comp #{:root :block} :tag)                                ; branch?
                    :children
                    (fn [node children] (assoc node :children (vec children))) ; make-node
                    root)))

(defn- children->then [node]
  (assert (not (:then node)) "There are multiple `else` for the same `if`.")
  (rename-keys node {:children :then}))

(defn- find-opening-block [zipper closing-node]
  (->> (iterate z/up zipper)
       (some (fn [z]
               (assert (some? z) "No opening block found.")
               (let [node (z/node z)]
                 (when (and (= (:tag node) :block)
                            (not (:did-not-open-a-block node)))
                   (assert (= (:block-type node)
                              (:block-type closing-node))
                           "The closing block does not match the opening block.")
                   z))))))

(defn- handlebars-zipper-reducer
  "Builds a tree-shaped representation of the handlebar's nodes."
  ([] (handlebars-zipper))
  ([zipper] zipper)
  ([zipper node]
   (case (:tag node)
     :open-block  (-> zipper
                      (z/append-child (assoc node :tag :block))
                      (z/down)
                      (z/rightmost))
     :else        (-> zipper
                      (z/edit children->then))
     :else-if     (-> zipper
                      (z/edit children->then)
                      (z/append-child (assoc node
                                             :tag :block
                                             :block-type :if
                                             :did-not-open-a-block true))
                      (z/down))
     :close-block (-> zipper
                      (find-opening-block node)
                      z/up)
     (z/append-child zipper node))))

;; TODO: support the `..`
(defn- to-data-template
  "Generates a data-template from a handlebars tree's node."
  [node]
  (let [{:keys [tag block-type content children]} node
        [arg0 arg1] content]
    (case tag
      :root (mapv to-data-template children)
      (:text :string-value) arg0
      :boolean-value (= arg0 "true")
      :number-value (parse-long arg0)
      :fn-call (let [[func & args] content]
                 (list* (symbol func) (map to-data-template args)))
      :dotted-term (if (= (count content) 1)
                     (list 'hopen/ctx (keyword arg0))
                     (list 'get-in 'hopen/ctx (mapv keyword content)))
      :hash-params (into {}
                         (comp (partition-all 2)
                               (map (fn [[k v]] [(keyword k) (to-data-template v)])))
                         content)
      :partial (list 'b/template
                     (keyword arg0)
                     (if arg1
                       (list 'merge 'hopen/ctx (to-data-template arg1))
                       'hopen/ctx))
      :block (case block-type
               :if     (if-let [then (seq (:then node))]
                         (list 'b/if (list 'hb/true? (to-data-template arg0))
                               (mapv to-data-template then)
                               (mapv to-data-template children))
                         (list 'b/if (list 'hb/true? (to-data-template arg0))
                               (mapv to-data-template children)))
               :unless (list 'b/if (list 'hb/false? (to-data-template arg0))
                             (mapv to-data-template children))
               :with   (list 'b/let ['hopen/ctx (to-data-template arg0)]
                             (mapv to-data-template children))
               :each   (if (= (:tag arg0) :each-as-args)
                         (let [[coll var index] (:content arg0)]
                           (list 'b/for ['hb/kv-pair (list 'hb/as-kvs (to-data-template coll))]
                                 [(list 'b/let ['hopen/ctx
                                                (list 'assoc 'hopen/ctx
                                                      (keyword index) '(first hb/kv-pair)
                                                      (keyword var) '(second hb/kv-pair))]
                                        (mapv to-data-template children))]))
                         (list 'b/for ['hopen/ctx (to-data-template arg0)]
                               (mapv to-data-template children)))
               ["Unhandled block-type:" node])
      ["Unhandled node type:" node])))

(defn parse [template]
  (-> (transduce (comp (part/template-partition part/default-delimiters)
                       (remove handlebars-comment?)
                       cleanup-text-segments
                       (map handlebars-node))
                 handlebars-zipper-reducer
                 [template])
      z/root
      to-data-template))

(defn- handlebars-false? [x]
  (or (not x)
      (and (string? x) (empty? x))
      (and (number? x) (zero? x))
      (and (coll? x) (empty? x))))

(defn- as-key-value-pairs [coll]
  (cond
    (map? coll) (seq coll)
    (coll? coll) (map-indexed vector coll)))

(defn with-handlebars-env [env]
  (update env :bindings assoc
          'hb/true? (comp not handlebars-false?)
          'hb/false? handlebars-false?
          'hb/as-kvs as-key-value-pairs))
