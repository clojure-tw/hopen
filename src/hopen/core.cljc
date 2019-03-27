(ns hopen.core
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
                                  (apply f env args)
                                  (if-let [f (get-in env [:bindings f-symb])]
                                    (apply f (mapv f-eval args))
                                    (throw (#?(:clj  Exception.
                                               :cljs js/Error.)
                                             (str "Function " f-symb " not found in env " env))))))
            :else element))]
   (f-eval element)))

;; Top level evaluation functions
;; The given element can be a "block" to be executed or an simple expression to be evaluated
(defn- rf-block [rf env result element]
  (or (when (list? element)
        (let [[f-symb & args] element
              f (get-in env [:block-macro f-symb])

              ;; Values in :block-macro could be a function or a var
              ;; If it's a var, resolve it now (into a function).
              ;; This allows us to frequently rebind/re-eval functions without
              ;; having to reconstruct `default-env`
              f (if-not (var? f)
                  f
                  (var-get f))]
          (when (fn? f)
            (apply f rf env result args))))
      (rf result (tpl-eval env element))))


(defn- parse-binding [bindings]
  (let [[k coll & next-bindings] bindings
        qualifiers (reduce (fn [qualifiers [k v]]
                             (if-not (keyword? k)
                               (reduced qualifiers)
                               (assoc qualifiers k v)))
                           {}
                           (partition 2 next-bindings))]
    [k coll qualifiers (drop (* 2 (count qualifiers)) next-bindings)]))

(defn parse-bindings [bindings]
  (loop [accum []
         remaining bindings]

    (if (empty? remaining)
      accum

      (let [[k coll qualifiers next-bindings :as single-binding] (parse-binding remaining)]
        (recur (conj accum (butlast single-binding))
               next-bindings)))))

(defn- rf-for [rf env result bindings content]
  (if (seq bindings)
    ;; Deconstruct one pair of the `for` binding
    (let [[k coll {separator :separated-by} next-bindings] (parse-binding bindings)

          ;; Evaluate the bound expression (into a collection) to be operate over
          loop-operands (tpl-eval env coll)]

      ;; Loop over the operands and build up the result
      ;; Note that here, we're inside a transducer and `result` is actually mutated.
      ;; So there is no difference between `result` and `next-result`.
      ;; We keep the code structure as if we're dealing with immutable datastrcuture
      ;; for the sake of clarity.
      (loop [result result
             remaining loop-operands]

        (if (empty? remaining)
          result

          (let [;; Bind a new value to the symbol...
                ;; Call `rf-for` recursively to deal with the rest of the bindings
                next-result (rf-for rf
                                    (update env :bindings assoc k (first remaining))
                                    result
                                    next-bindings
                                    content)

                ;; If a separator has been provided
                ;; and we're not looking at the last item in the list,
                ;; insert the separator now
                next-result (if (and separator (not (empty? (rest remaining))))
                              (apply rf next-result (tpl-eval env separator))
                              next-result)]
            (recur next-result
                   (rest remaining))))))

    ;; No more bindings to deal with.
    ;; At this point, all required symbols should have been bound to a specific value
    ;; We should be able to just evaluate the remaining content.
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

(defn- inline-for [env bindings content]
  (letfn [(rf-for [rf env result bindings]
            (if (seq bindings)
              (let [[k coll & next-bindings] bindings]
                (reduce (fn [result bound-val]
                          (rf-for rf
                                  (update env :bindings assoc k bound-val)
                                  result
                                  next-bindings))
                        result
                        (tpl-eval env coll)))
              (rf result (tpl-eval env content))))]
    (rf-for conj env '[] bindings)))

(defn- inline-let [env bindings content]
  ;; Build a new env using the given bindings
  (let [env (reduce (fn [env [symb bound-expr]]
                      ;; For each pair in the binding form...
                      ;; Evaluate bound expression, then
                      ;; Push the value into env with the correct symbol
                      (update env :bindings assoc symb (tpl-eval env bound-expr)))
                    env
                    (partition 2 bindings))]

    ;; Continue evaluation with the new env
    (tpl-eval env content)))

(defn- inline-if
  ([env cond then] (inline-if env cond then nil))
  ([env cond then else]
   (if (tpl-eval env cond) (tpl-eval env then) (tpl-eval env else))))

(defn- inline-quote [env val]
  val)

;; An `env` maps an op in a template to its actual handler
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
   {'for   inline-for
    'let   inline-let
    'if    inline-if
    'quote inline-quote}

   ;; Contains:
   ;; - the inline functions,
   ;; - 'hopen/root, points to the root of the template's data, shall not be redefined.
   ;; - 'hopen/ctx, also points to the template's data, can be locally redefined.
   :bindings
   {'get-in get-in
    'range  range

    'first first
    'last  last
    'conj  conj
    'pop   pop

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

  ;; `renderer` returns a transducer
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
