(ns hopen.syntax.util
  (:require [clojure.string :as str]))

(defn re-quote
  "Escapes characters in the string that are not safe to use in a regex.
   Function ported to Clojure from https://github.com/google/closure-library/blob/0257667129eded0cb9b86b4701e50c986b5db648/closure/goog/string/string.js#L1016"
  [s]
  (-> s
      (str/replace #"[-()\[\]{}+?*.$\^|,:#<!\\]" #(str "\\" %))
      (str/replace #"\x08", "\\x08")))
