(ns hopen.util)

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

(defn collect [data keys]
  (into [] (keep #(get data %) keys)))

(defn collect-in [data path]
  (when (seq path)
    (reduce (fn [coll keys]
              (into []
                    (mapcat (fn [item] (collect item keys)))
                    coll))
            [data]
            path)))
