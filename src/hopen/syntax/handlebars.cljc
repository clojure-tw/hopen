(ns hopen.syntax.handlebars
  (:require [clojure.string :as str]
            [hopen.syntax.util :refer [re-quote]]
            [hopen.util :refer [throw-exception]]
            [instaparse.gll :refer [text->segment sub-sequence]]))

(def default-delimiters {:open "{{", :close "}}"})

(def ^:private close-delim-not-found-msg
  (str "The end of the template has been reached, "
       "but the closing delimiter was not found!"))

(defn- parse-change-delim
  "Parses a change-delimiter tag, returns nil if it was not one,
  returns `[matched-text open-delim close-delim]` otherwise."
  [segment close-delim]
  (let [re (re-pattern (str "^"
                            (re-quote "=")
                            "\\s+(\\S+)\\s+(\\S+)\\s+"
                            (re-quote "=")
                            (re-quote close-delim)))]
    (re-find re segment)))

(defn- parse-text-segments
  "Parses and collects all the text segments until a syntax block is found.

  This function handles and interprets the tags which are changing the
  delimiters, so that the rest of the program doesn't have to deal with it.

  Returns `[text-segments next-segment open-delim close-delim]`."
  [segment open-delim close-delim]
  (loop [text-segments []
         segment segment
         open-delim open-delim
         close-delim close-delim]
    (if-let [index (str/index-of segment open-delim)]
      (let [text-segments (if (zero? index)
                            text-segments
                            (conj text-segments (sub-sequence segment 0 index)))
            syntax-segment (sub-sequence segment (+ index (count open-delim)))]
        ;; Is it a change-delimiter tag?
        (if-let [change-delimiters (parse-change-delim syntax-segment close-delim)]
          ;; Yes it is, so we should continue to parse more text segments.
          (let [[matched-text open-delim close-delim] change-delimiters]
            (recur text-segments
                   (sub-sequence syntax-segment (count matched-text))
                   open-delim
                   close-delim))
          ;; No it's a syntax segment, so we are done parsing text segments.
          [text-segments syntax-segment open-delim close-delim]))
      ;; The whole remaining text is the text segment, we are done.
      [(if (zero? (count segment))
         text-segments
         (conj text-segments segment))
       nil
       open-delim
       close-delim])))

(defn- parse-syntax-segment
  "Parses the syntax segment, assuming that this function is provided a segment
  at the start of that syntax segment.

  Returns `[syntax-segment next-segment]`."
  [segment close-delim]
  (if-let [index (str/index-of segment close-delim)]
    [(sub-sequence segment 0 index)
     (sub-sequence segment (+ index (count close-delim)))]
    (throw-exception close-delim-not-found-msg)))

(defn partition-template [delimiters]
  (fn [rf]
    (fn
      ([] (rf))
      ([result] (rf result))
      ([result template]
       (loop [result result
              segment (text->segment template)
              open-delim (:open delimiters)
              close-delim (:close delimiters)]
         ;; Read the text segments.
         (let [[text-segments next-segment open-delim close-delim]
               (parse-text-segments segment open-delim close-delim)

               result (cond-> result
                        (seq text-segments) (rf [:text text-segments]))]
           (if next-segment
             ;; Read the syntax segment.
             (let [[syntax-segment next-segment]
                   (parse-syntax-segment next-segment close-delim)]
               (recur (rf result [:syntax syntax-segment])
                      next-segment
                      open-delim
                      close-delim))
             ;; No more segments to read.
             result)))))))

(comment
  (into []
        (partition-template default-delimiters)
        [""])

  (into []
        (partition-template default-delimiters)
        ["aa bb"])

  (into []
        (partition-template default-delimiters)
        ["aa {{= < > =}} bb"])

  (into []
        (partition-template default-delimiters)
        ["aa {{= | | =}} bb |cc| dd"]))

;; Simple implementation for now.
(defn compile-to-data-template [[type val :as segment]]
  (case type
    :text (apply str (map str val))
    :syntax (list 'hopen/ctx (keyword (str/trim val)))))

(defn parser [template]
  (into []
        (comp (partition-template default-delimiters)
              (map compile-to-data-template))
        [template]))
