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

(defn- rf-block [rf env result element]
  (or (when (list? element)
        (let [[f-symb & args] element
              f (get-in env [:block-macro f-symb])]
          (when f
            (apply f rf env result args))))
      (rf result (tpl-eval env element))))

(defn- binding-partition
  "A transducer which is partitioning a multi-variables binding sequence."
  [rf]
  (let [state (volatile! [])]
    (fn
      ([] (rf))
      ([result] (let [binding @state]
                  (rf (cond-> result
                        (seq binding) (rf binding)))))
      ([result input]
       (let [binding @state
             length (count binding)]
         (if (and (even? length)
                  (>= length 2)
                  (not (keyword? input)))
           (do (vreset! state [input])
               (rf result binding))
           (do (vswap! state conj input)
               result)))))))

(defn- parse-bindings [bindings]
  (into []
        (comp binding-partition
              (map (fn [binding]
                     (let [[symb value & {:as options}] binding]
                       [symb value options]))))
        bindings))

(defn- inter-reduce [rf-items rf-separators result coll]
  (if (seq coll)
    (reduce (fn [result item]
              (-> result
                  (rf-separators)
                  (rf-items item)))
            (rf-items result (first coll))
            (next coll))
    result))

(defn- rf-for [rf env result bindings-seq content]
  (letfn [(for-binding [env result bindings]
            (if (seq bindings)
              (let [[[symb coll {:keys [separated-by]}] & next-bindings] bindings]
                (inter-reduce (fn [result val]
                                  (for-binding (update env :bindings assoc symb val)
                                               result
                                               next-bindings))
                              (fn [result]
                                (reduce (partial rf-block rf env)
                                        result
                                        separated-by))
                              result
                              (tpl-eval env coll)))
              (reduce (partial rf-block rf env)
                      result
                      content)))]
    (for-binding env result (parse-bindings bindings-seq))))

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
  (let [env (reduce (fn [env [symb bound-expr]]
                      (update env :bindings assoc symb (tpl-eval env bound-expr)))
                    env
                    (partition 2 bindings))]
    (tpl-eval env content)))

(defn- inline-if
  ([env cond then] (inline-if env cond then nil))
  ([env cond then else]
   (if (tpl-eval env cond) (tpl-eval env then) (tpl-eval env else))))

(defn- inline-quote [env val]
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
   {'for   inline-for
    'let   inline-let
    'if    inline-if
    'quote inline-quote}

   ;; Contains:
   ;; - the inline functions,
   ;; - 'hopen/root, points to the root of the template's data, shall not be redefined.
   ;; - 'hopen/ctx, also points to the template's data, can be locally redefined.
   :bindings
   {'inline identity
    'get-in get-in
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
