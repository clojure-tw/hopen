(ns hopen.renderer.env
  (:require [clojure.string :as str]
            [hopen.util :as util]))

(def standard-env
  {;; The templates accessible inside the current template.
   :templates
   {}

   ;; The block-macro functions get their args unevaluated and render blocks of elements in-place.
   :block-macro
   {'b/for       nil
    'b/let       nil
    'b/if        nil
    'b/cond      nil
    'b/interpose nil
    'b/template  nil}

   ;; The inline-macro functions get their args unevaluated and render an element.
   :inline-macro
   {'for   nil
    'let   nil
    'if    nil
    'cond  nil
    'quote nil}

   ;; Contains:
   ;; - the inline functions,
   ;; - 'hopen/parent-root, points to the parent template's hopen/root when in a nested template, shall not be redefined.
   ;; - 'hopen/root, points to the root of the template's data, shall not be redefined.
   ;; - 'hopen/ctx, also points to the template's data, can be locally redefined.
   :bindings
   {;; Get things
    'get        get
    'get-in     get-in
    'collect    util/collect
    'collect-in util/collect-in
    'first      first
    'next       next
    'last       last
    'pop        pop
    'count      count

    ;; Alter collections
    'cons   cons
    'conj   conj
    'assoc  assoc
    'dissoc dissoc
    'concat concat
    'merge  merge

    ;; Build sequences
    'list          list
    'take          take
    'drop          drop
    'map           map
    'comp          comp
    'range         range
    'cycle         cycle
    'constantly    constantly
    'partition     partition
    'partition-all partition-all

    ;; Some maths
    'inc inc
    'dec dec
    '+   +
    '-   -
    '*   *
    '/   /
    'mod mod

    ;; Compare numbers
    '<     <
    '<=    <=
    '>     >
    '>=    >=
    '=     =
    'not=  not=
    'neg?  neg?
    'zero? zero?
    'pos?  pos?

    ;; Test the data types
    'boolean? boolean?
    'number?  number?
    'string?  string?
    'coll?    coll?
    'seq?     seq?
    'list?    list?
    'vector?  vector?
    'map?     map?
    'sorted?  sorted?
    'set?     set?
    'record?  record?

    ;; Text transformations
    'str   str
    'join  str/join
    'cap   str/capitalize
    'upper str/upper-case
    'lower str/lower-case

    'empty?  empty?
    'not     not}})
