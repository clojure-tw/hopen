(ns hopen.util
  (:require [clojure.string :as str]))

(defn triml
  "Trims the white spaces at the beginning of each line in the text, including the delimiter."
  ([text] (triml text "|"))
  ([text delimiter]
   (transduce (comp (map (fn [line]
                           (let [trimmed (str/triml line)]
                             (if (str/starts-with? trimmed delimiter)
                               (subs trimmed (count delimiter))
                               line))))
                    (interpose "\n"))
              str
              (str/split-lines text))))

(defn binding-partition
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

(defn parse-bindings [bindings]
  (into []
        (comp binding-partition
              (map (fn [binding]
                     (let [[symb value & {:as options}] binding]
                       [symb value options]))))
        bindings))

(defn parse-long [s]
  #?(:cljs (js/parseInt s)
     :clj (Long/parseLong s)))

(defn update-existing
  "Updates the map only on the keys that already exist in the map."
  [m k f & args]
  (if (contains? m k)
    (apply update m k f args)
    m))
