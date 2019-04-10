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

(defn throw-exception [message]
  (throw (#?(:clj  Exception.
             :cljs js/Error.) message)))

(defn escape-html [text]
  #?(:cljs
     ;; Ripped from https://stackoverflow.com/a/9677462/605323
     (.replace text (js/RegExp "[&<>\"']" "g") (fn [k] (aget (js-obj "&" "&amp;"
                                                                    "<" "&lt;"
                                                                    ">" "&gt;"
                                                                    "\"" "&quot;"
                                                                    "'" "&#039;")
                                                            k)))
     :clj
     ;; Ripped from hiccup https://github.com/weavejester/hiccup/blob/abc97943ee0fb72e5c94f8fac170be5535f9f2d4/src/hiccup/util.clj#L80
     ;; This seems like it would be somewhat "slow". It scans the string multiple times & each time generating a new string.
     ;; It's probably fine for short strings.
     (.. ^String text
         (replace "&"  "&amp;")
         (replace "<"  "&lt;")
         (replace ">"  "&gt;")
         (replace "\"" "&quot;")
         (replace "'" "&#39;"))))
