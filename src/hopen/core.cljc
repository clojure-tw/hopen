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
                 :get    (-> env
                             (get (first args))
                             (get (second args)))
                 :get-in (-> env
                             (get (first args))
                             (get-in (second args)))
                 :fn     (let [f (fns (first args))
                               f-args (mapv (partial tpl-eval env)
                                            (rest args))]
                           (apply f f-args))))
             (let-rf [env result bindings content]
               (if (seq bindings)
                 (let [[k val & next-bindings] bindings]
                   (let-rf (assoc env k (tpl-eval env val))
                           result
                           next-bindings
                           content))
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
