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
     (fn
       ([] (rf))
       ([result] (rf result))
       ([result input]
        (let [env {'hopen/root input
                   'hopen/ctx input}
              tpl-eval
              (fn tpl-eval [env [op & args]]
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
              block-rf
              (fn block-rf [env result [op & args :as element]]
                (case op
                  ;; Block functions are handled here.

                  ;; TODO: implement the binding in env.
                  :let (let [bindings (first args)
                             env (into env
                                       (comp (partition-all 2)
                                             (map (fn [[k v]]
                                                    [k (tpl-eval env v)])))
                                       bindings)]
                         (reduce (partial block-rf env)
                                 result
                                 (second args)))

                  ;; Else clause is for the non-block operations
                  ;; which evaluate without needing the rf parameter.
                  (rf result (tpl-eval env element))))]
          (reduce (partial block-rf env)
                  result
                  tpl)))))))
