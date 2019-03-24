(ns hopen.core
  (:refer-clojure :exclude [compile])
  (:require [clojure.string :as str]))

(def built-in-functions
  {:inc   inc
   :dec   dec
   :add   +
   :sub   -
   :mul   *
   :div   /
   :mod   mod

   :str   str
   :join  str/join
   :upper str/upper-case
   :lower str/lower-case

   ,,,})

(defn renderer
  ([tpl]
   (renderer tpl built-in-functions))
  ([tpl fns]
   (fn [rf]
     (letfn [(tpl-eval [env [op & args]]
               (case op
                 :value  (first args)
                 :get    (let [[symb key] args]
                           (-> env
                               (get symb)
                               (get key)))
                 :get-in (let [[symb path] args]
                           (-> env
                               (get symb)
                               (get-in path)))
                 :fn     (let [[f-key & f-args] args
                               f (get fns f-key)
                               f-args (mapv (partial tpl-eval env) f-args)]
                           (apply f f-args))))
             (let-rf [env result bindings content]
               (let [env (reduce (fn [env [symb val]]
                                   (assoc env symb (tpl-eval env val)))
                                 env
                                 (partition-all 2 bindings))]
                 (reduce (partial block-rf env)
                         result
                         content)))
             (for-rf [env result bindings content]
               (if (seq bindings)
                 (let [[k coll & next-bindings] bindings]
                   (reduce (fn [result val]
                             (for-rf (assoc env k val)
                                     result
                                     next-bindings
                                     content))
                           result
                           (tpl-eval env coll)))
                 (reduce (partial block-rf env)
                         result
                         content)))
             (block-rf [env result [op & args :as element]]
               (case op
                 ;; Block functions are handled here.
                 :let (let [[bindings content] args]
                        (let-rf env result bindings content))
                 :for (let [[bindings content] args]
                        (for-rf env result bindings content))

                 ;; Else clause is for the non-block operations
                 ;; which evaluate without needing the rf parameter.
                 (rf result (tpl-eval env element))))]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (let [env {'hopen/root input
                     'hopen/ctx input}]
            (reduce (partial block-rf env)
                    result
                    tpl))))))))
