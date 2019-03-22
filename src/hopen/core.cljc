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
              (fn tpl-eval [[op & args]]
                (case op
                 :value  (first args)
                 :get    (-> env
                             (get (first args))
                             (get (second args)))
                 :get-in (-> env
                             (get (first args))
                             (get-in (second args)))
                 :fn     (let [f (fns (first args))
                               f-args (mapv tpl-eval (rest args))]
                           (apply f f-args))))]
          (reduce
            (fn block-rf [result [op & args :as element]]
              (case op
                ;; Block function are handled here.

                ;; TODO: implement the binding in env.
                :let (reduce block-rf result (second args))

                ;; Else clause is for the non-block operations
                ;; which evaluate without needing the rf parameter.
                (rf result (tpl-eval element))))
            result
            tpl)))))))
