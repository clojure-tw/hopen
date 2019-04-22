(ns hopen.syntax.handlebars
  (:require [clojure.set :refer [rename-keys]]
            [clojure.walk :refer [postwalk]]
            [clojure.zip :as z]
            [hopen.syntax.partition :as part]
            [hopen.util :refer [parse-long update-existing]]
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
   dotted-term = '../'* (!keyword symbol (<'.'> symbol)* | '@index' | '@key')
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
  ([] (handlebars-zipper {:tag :block, :block-type :root}))
  ([root] (z/zipper (fn [node] (= (:tag node) :block))                         ; branch?
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
                      (z/up))
     (z/append-child zipper node))))

(defn- assoc-nesting
  "Add context nesting information to context-altering blocks and dotted-terms."
  [root-node]
  (letfn [(simplify-dotted-term [{:keys [content ctx-nesting] :as node}]
            (let [n (count (take-while #{"../"} content))]
              (assoc node
                :content (drop n content)
                :ctx-nesting (- ctx-nesting n))))

          (assoc-nesting-to-dotted-terms [nesting]
            (fn [node]
              (postwalk (fn [n]
                          (cond-> n
                            (and (map? n)
                                 (#{:dotted-term :partial} (:tag n))) (-> (assoc :ctx-nesting nesting)
                                                                          (simplify-dotted-term))))
                        node)))

          (assoc-nesting-to-block [nesting]
            (fn [node]
              (if (= (:tag node) :block)
                (case (:block-type node)
                  (:if :unless)
                  (-> node
                      (update          :content  (assoc-nesting-to-dotted-terms nesting))
                      (update-existing :then     (assoc-nesting-to-blocks nesting))
                      (update          :children (assoc-nesting-to-blocks nesting)))
                  (:with :each)
                  (-> node
                      (assoc :ctx-nesting (inc nesting))
                      (update :content  (assoc-nesting-to-dotted-terms nesting))
                      (update :children (assoc-nesting-to-blocks (inc nesting)))))
                ((assoc-nesting-to-dotted-terms nesting) node))))

          (assoc-nesting-to-blocks [nesting]
            (fn [blocks]
              (mapv (assoc-nesting-to-block nesting) blocks)))]

    (-> root-node
        (assoc :ctx-nesting 0)
        (update :children (assoc-nesting-to-blocks 0)))))

(defn assoc-references-to-context-blocks [root-node]
  (postwalk (fn [node]
              (cond-> node
                (and (map? node) (#{:root :with :each} (:block-type node)))
                (assoc :refs (->> (tree-seq coll?
                                            (fn [x] (cond-> x
                                                      (map? x) (vals)))
                                            node)
                                  (filter (fn [x] (and (map? x)
                                                       (= (:tag x) :dotted-term)
                                                       (= (:ctx-nesting x) (:ctx-nesting node)))))
                                  (into [])))))
            root-node))

(defn- ctx-symbol [nesting-level]
  (if (zero? nesting-level)
    'hopen/root
    (symbol "hb" (str "ctx" nesting-level))))

(defn- loop-index-symbol [nesting-level]
  (symbol "hb" (str "index" nesting-level)))

(defn- loop-key-symbol [nesting-level]
  (symbol "hb" (str "key" nesting-level)))

(defn- to-data-template
  "Generates a data-template from a handlebars tree's node."
  [node]
  (let [{:keys [tag block-type content children ctx-nesting]} node
        [arg0 arg1] content
        ctx-symb (some-> ctx-nesting (ctx-symbol))]
    (case tag
      (:text :string-value) arg0
      :boolean-value (= arg0 "true")
      :number-value (parse-long arg0)
      :fn-call (let [[func & args] content]
                 `(~(symbol func) ~@(map to-data-template args)))
      :dotted-term (case arg0
                     "@index" (loop-index-symbol ctx-nesting)
                     "@key" (loop-key-symbol ctx-nesting)
                     (if (= (count content) 1)
                       `(~ctx-symb ~(keyword arg0))
                       `(~'get-in ~ctx-symb ~(mapv keyword content))))
      :hash-params (into {}
                         (comp (partition-all 2)
                               (map (fn [[k v]] [(keyword k) (to-data-template v)])))
                         content)
      :partial `(~'b/template ~(keyword arg0)
                              ~(if arg1
                                 `(~'merge ~ctx-symb ~(to-data-template arg1))
                                 ctx-symb))
      :block (case block-type
               :root   (mapv to-data-template children)
               :if     (if-let [then (seq (:then node))]
                         `(~'b/if (~'hb/true? ~(to-data-template arg0))
                            ~(mapv to-data-template then)
                            ~(mapv to-data-template children))
                         `(~'b/if (~'hb/true? ~(to-data-template arg0))
                            ~(mapv to-data-template children)))
               :unless `(~'b/if (~'hb/false? ~(to-data-template arg0))
                          ~(mapv to-data-template children))
               :with   `(~'b/let [~ctx-symb ~(to-data-template arg0)]
                          ~(mapv to-data-template children))
               :each   (let [loop-index? (some (comp #{["@index"]} :content) (:refs node))
                             loop-key?   (some (comp #{["@key"]}   :content) (:refs node))
                             index-options (when loop-index? [:indexed-by (loop-index-symbol ctx-nesting)])
                             key-options   (when loop-key?   [(loop-key-symbol ctx-nesting) '(first hb/pair)])]
                         (if (= (:tag arg0) :each-as-args)
                           (let [[coll var index] (:content arg0)]
                             `(~'b/for [~'hb/pair (~'hb/as-kvs ~(to-data-template coll)) ~@index-options]
                                [(~'b/let [~ctx-symb (~'assoc ~(ctx-symbol (dec ctx-nesting))
                                                       ~(keyword index) ~'(first hb/pair)
                                                       ~(keyword var) ~'(second hb/pair))
                                           ~@key-options]
                                   ~(mapv to-data-template children))]))
                           `(~'b/for [~'hb/pair (~'hb/as-kvs ~(to-data-template arg0)) ~@index-options]
                                  [(~'b/let [~ctx-symb ~'(second hb/pair)
                                             ~@key-options]
                                     ~(mapv to-data-template children))])))
               ["Unhandled block-type:" node])
      ["Unhandled node type:" node])))

(defn parse [template]
  (-> (transduce (comp (part/template-partition part/default-delimiters)
                       (remove handlebars-comment?)
                       cleanup-text-segments
                       (map handlebars-node))
                 handlebars-zipper-reducer
                 [template])
      (z/root)
      (assoc-nesting)
      (assoc-references-to-context-blocks)
      (to-data-template)))

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
