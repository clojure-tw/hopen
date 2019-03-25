(ns hopen.core
  (:refer-clojure :exclude [compile])
  (:require [clojure.string :as str]))

;; rf-fn are block functions that get the args unevaluated, similarly to macros.
(defn- rf-fn? [f]
  (:rf-fn (meta f)))

(defn- tpl-eval [env element]
  (letfn [(f-eval [element]
           (cond
            (symbol? element) (env element)
            (vector? element) (into [] (map f-eval) element)
            (set? element)    (into #{} (map f-eval) element)
            (map? element)    (into {} (map (fn [[k v]] [(f-eval k) (f-eval v)])) element)
            (list? element)   (let [[f-symb & args] element]
                               (apply (env f-symb) (mapv f-eval args)))
            :else element))]
   (f-eval element)))

(defn- rf-block [rf env result element]
  (or (when (list? element)
        (let [[f-symb & args] element
              f (env f-symb)]
          (when (rf-fn? f)
            (apply f rf env result args))))
      (rf result (tpl-eval env element))))

(def ^:private rf-for ^:rf-fn
  (fn [rf env result bindings content]
    (if (seq bindings)
      (let [[k coll & next-bindings] bindings]
        (reduce (fn [result val]
                  (rf-for rf
                          (assoc env k val)
                          result
                          next-bindings
                          content))
                result
                (tpl-eval env coll)))
      (reduce (partial rf-block rf env)
              result
              content))))

(def ^:private rf-let ^:rf-fn
  (fn [rf env result bindings content]
    (let [env (reduce (fn [env [symb val]]
                        (assoc env symb (tpl-eval env val)))
                      env
                      (partition 2 bindings))]
      (reduce (partial rf-block rf env)
              result
              content))))

(def ^:private rf-quote ^:rf-fn
  (fn [rf env result content]
    (rf result content)))

(def default-env
  {;; Block functions
   'for rf-for
   'let rf-let
   'quote rf-quote

   ;; Inline functions
   'get-in get-in

   'inc   inc
   'dec   dec
   '+   +
   '-   -
   '*   *
   '/   /
   'mod   mod

   'str   str
   'join  str/join
   'upper str/upper-case
   'lower str/lower-case})

(defn renderer
  ([tpl] (renderer tpl default-env))
  ([tpl env] (fn [rf]
               (fn
                 ([] (rf))
                 ([result] (rf result))
                 ([result input]
                  (let [env (assoc env
                              'hopen/root input
                              'hopen/ctx input)]
                    (reduce (partial rf-block rf env)
                            result
                            tpl)))))))
