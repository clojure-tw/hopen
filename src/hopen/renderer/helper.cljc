(ns hopen.renderer.helper)

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
