(ns hopen.syntax.handlebars
  (:require [clojure.set :refer [rename-keys]]
            [clojure.string :as str]
            [clojure.zip :as z]
            [hopen.syntax.util :refer [re-quote]]
            [hopen.util :refer [throw-exception triml]]
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
;; removes blank texts,
;; removes empty text segments.
(def ^:private cleanup-text-segments
  (comp (partition-by first)
        (mapcat (fn [[[type] :as coll]]
                  (if (= type :text)
                    (let [segments (into []
                                         (comp (mapcat second)
                                               (remove str/blank?))
                                         coll)]
                      (when (seq segments)
                        [[:text segments]]))
                    coll)))))

(declare handlebars-args)

(defn- handlebars-deref-expression [s]
  {:type :deref
   :fields (str/split s #"\.")})

(defn- handlebars-expression-group [s]
  (let [[_ part0 next-parts] (re-matches #"\s*(\S+)\s*(.*)" s)]
    (if (str/blank? next-parts)
      (handlebars-deref-expression part0)
      {:type :call
       :func part0
       :args (handlebars-args next-parts)})))

(comment
  (handlebars-expression-group "aa.bb")
  (handlebars-expression-group "aa bb.cc dd.ee.ff"))

;; TODO: support hash arguments.
(defn- handlebars-args [s]
  ;; TODO: support recursive parenthesis grouping.
  (let [args (->> (str/split (str/trim s) #"\s+")
                  (remove str/blank?))]
    (mapv handlebars-expression-group args)))

;; TODO: being robust to multiline expressions.
;; TODO: partial templates.
;; TODO: support Handlebars' special syntax of the #each block.
(defn- handlebars-node
  "Returns a handlebars node from an element of the segment partition."
  [[type segment]]
  (case type
    :text {:type :text
           :value (apply str (map str segment))}
    :syntax (if-let [[_ block-name exprs] (re-matches #"\#(\S+)\s*(.*)" segment)]
              {:type (keyword block-name)
               :open-block? true
               :args (handlebars-args exprs)}

              (if (re-matches #"else\s*" segment)
                {:type :else}

                (if-let [[_ exprs] (re-matches #"else\s+if\s+(.*)" segment)]
                  {:type :else-if
                   :args (handlebars-args exprs)}

                  (if-let [[_ block-name] (re-matches #"/(\S+)\s*" segment)]
                    {:type (keyword block-name)
                     :close-block? true}

                    (handlebars-expression-group segment)))))))

(defn- handlebars-zipper
  ([] (handlebars-zipper {:type :root, :branch? true}))
  ([root] (z/zipper :branch?
                    :children
                    (fn [node children] ; make-node
                      (assoc node :children (vec children)))
                    root)))

;; TODO: check if the closing block matches the opening block.
;; TODO: handle the error when no opening block is found.
(defn- handlebars-zipper-reducer
  "Builds a tree-shaped representation of the handlebar's nodes."
  ([] (handlebars-zipper))
  ([zipper] zipper)
  ([zipper node]
   (cond
     (:open-block? node) (-> zipper
                             (z/append-child (assoc node :branch? true))
                             (z/down)
                             (z/rightmost))
     (= (:type node) :else) (-> zipper
                                (z/edit assoc :type :if-then-else)
                                (z/edit rename-keys {:children :then}))
     (= (:type node) :else-if) (-> zipper
                                (z/edit assoc :type :if-then-else)
                                (z/edit rename-keys {:children :then})
                                (z/append-child (assoc node
                                                  :type :if
                                                  :branch? true))
                                (z/down))
     (:close-block? node) (-> (some #(when (:open-block? (z/node %)) %)
                                    (iterate z/up zipper))
                              (z/edit dissoc :open-block?)
                              z/up)
     :else (z/append-child zipper node))))

;; TODO: support the `..`
(defn- to-data-template
  "Generates a data-template from a handlebars tree's node."
  [node]
  (case (:type node)
    :root (mapv to-data-template (:children node))
    :text (:value node)
    :call (let [{:keys [func args]} node]
            (list* (symbol func) (map to-data-template args)))
    :deref (let [[field0 & next-fields :as fields] (:fields node)]
             (if (nil? next-fields)
               (list 'hopen/ctx (keyword field0))
               (list 'get-in 'hopen/ctx (mapv keyword fields))))
    :if (list 'b/if (to-data-template (-> node :args first))
              (mapv to-data-template (:children node)))
    :if-then-else (list 'b/if (to-data-template (-> node :args first))
                        (mapv to-data-template (:then node))
                        (mapv to-data-template (:children node)))
    :with (list 'b/let ['hopen/ctx (to-data-template (-> node :args first))]
                (mapv to-data-template (:children node)))
    :each (list 'b/for ['hopen/ctx (to-data-template (-> node :args first))]
                (mapv to-data-template (:children node)))
    ["Unhandled:" node]))

(defn parse [template]
  (-> (transduce (comp (template-partition default-delimiters)
                       (remove handlebars-comment?)
                       cleanup-text-segments
                       (map handlebars-node))
                 handlebars-zipper-reducer
                 [template])
      z/root
      to-data-template))


(comment
  ;; The Handlebars' syntax:

  "! this is a comment"
  "!-- this is a comment --"

  "#each movie.staffs"
  "/each"

  "#if movie"
  "else if person"
  "else"
  "/if"

  "full-name-helper person arg1=val1 arg2=val2 ..."

  "> sub-template var1=val1 var2=val2 ...")
