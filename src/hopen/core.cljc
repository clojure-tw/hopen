(ns hopen.core
  (:refer-clojure :exclude [compile])
  (:require [clojure.string :as str]))

(defn- tpl-eval [env element]
  (letfn [(f-eval [element]
           (cond
            (symbol? element) (get-in env [:bindings element])
            (vector? element) (into [] (map f-eval) element)
            (set? element)    (into #{} (map f-eval) element)
            (map? element)    (into {} (map (fn [[k v]] [(f-eval k) (f-eval v)])) element)
            (list? element)   (let [[f-symb & args] element]
                                (if-let [f (get-in env [:inline-macro f-symb])]
                                  (apply f f-eval args)
                                  (if-let [f (get-in env [:bindings f-symb])]
                                    (apply f (mapv f-eval args))
                                    (throw (#?(:clj  Exception.
                                               :cljs js/Error.)
                                             (str "Function " f-symb " not found in env " env))))))
            :else element))]
   (f-eval element)))

(defn- rf-block [rf env result element]
  (or (when (list? element)
        (let [[f-symb & args] element
              f (get-in env [:block-macro f-symb])]
          (when f
            (apply f rf env result args))))
      (rf result (tpl-eval env element))))

(defn- rf-for [rf env result bindings content]
  (if (seq bindings)
    (let [[k coll & next-bindings] bindings]
      (reduce (fn [result val]
                (rf-for rf
                        (update env :bindings assoc k val)
                        result
                        next-bindings
                        content))
              result
              (tpl-eval env coll)))
    (reduce (partial rf-block rf env)
            result
            content)))

(defn- rf-let [rf env result bindings content]
  (let [env (reduce (fn [env [symb val]]
                      (update env :bindings assoc symb (tpl-eval env val)))
                    env
                    (partition 2 bindings))]
    (reduce (partial rf-block rf env)
            result
            content)))

(defn- rf-if
  ([rf env result cond then]
   (rf-if rf env result cond then nil))
  ([rf env result cond then else]
   (reduce (partial rf-block rf env)
           result
           (if (tpl-eval env cond) then else))))

(defn- rf-quote [rf env result content]
  (rf result content))

;; Note: the separator is evaluated multiple times.
;; Use a `let` if you need to reduce the performance impact.
(defn- rf-interpose [rf env result separator content]
  (reduce ((interpose separator)
           (partial rf-block rf env))
          result
          content))

(defn- inline-if
  ([f-eval cond then] (inline-if f-eval cond then nil))
  ([f-eval cond then else] (if (f-eval cond) (f-eval then) (f-eval else))))

(defn- inline-quote [f-eval val]
  val)

(def default-env
  {;; Block-macro functions are reducer functions which get their args unevaluated.
   :block-macro
   {'for   rf-for
    'let   rf-let
    'if    rf-if
    'quote rf-quote

    ;; Based on transducers
    'interpose rf-interpose}

   ;; The inline-macro functions get their args unevaluated.
   :inline-macro
   {'if    inline-if
    'quote inline-quote}

   ;; Contains:
   ;; - the inline functions,
   ;; - 'hopen/root, points to the root of the template's data, shall not be redefined.
   ;; - 'hopen/ctx, also points to the template's data, can be locally redefined.
   :bindings
   {'get-in get-in
    'range  range

    'inc inc
    'dec dec
    '+   +
    '-   -
    '*   *
    '/   /
    'mod mod

    '<    <
    '<=   <=
    '>    >
    '>=   >=
    '=    =
    'not= not=

    'str   str
    'join  str/join
    'upper str/upper-case
    'lower str/lower-case}})

(defn renderer
  ([tpl] (renderer tpl default-env))
  ([tpl env] (fn [rf]
               (fn
                 ([] (rf))
                 ([result] (rf result))
                 ([result input]
                  (let [env (update env :bindings assoc
                              'hopen/root input
                              'hopen/ctx input)]
                    (reduce (partial rf-block rf env)
                            result
                            tpl)))))))
