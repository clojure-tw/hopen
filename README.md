# hopen 『活板』

[![Build Status](https://api.travis-ci.org/clojure-tw/hopen.svg?branch=master)](https://travis-ci.org/clojure-tw/hopen)
[![CircleCI](https://circleci.com/gh/clojure-tw/hopen/tree/master.svg?style=svg)](https://circleci.com/gh/clojure-tw/hopen/tree/master)

A simple, modern, flexible and portable template engine for Clojure environments.

# Get started

The API is very simple.

```clojure
(render "Hello {@:name}" {:name "john"})
; "Hello john"

(def parsed-template (parse-template "Hello {@:name}")

parsed-template
; [[:hopen/text "Hello "]
;  [:hopen/env [:name]]]

(render parsed-template {:name "john"})
; "Hello john"
```

# Default Template Syntax

This syntax is designed to be familiar to Clojure developers.

```clojure
; Start and end of template expressions.
{...}
{@:name}

; Replace expression delimiters with something else.
{}! "(" ")"    ; It feels like in Clojure.
{}! "<" ">"    ; Possible when you are not rendering html.
{}! "%" "%"    ; They can be the same on each side.
{}! "{{" "}}"  ; Not limited to a char.
{}! "{[" "]}"
{}! "%[" "]%"

; Replace expression delimiters back to their default.
{}! "{" "}"

; Note: the parenthesis usage is similar to Clojure, except that they are
;       implicit at the root level when there are more than 1 element.

; an immediate value, can be any clojure value except functions.
42
[1 5 7 {"hello" true}]

; data at provided path in the root context of the document.
_@@[:persons 0 :name]
_@:persons

; data at provided relative path in the current context.
@@[:persons 0 :name]
@:name

; thread operation
-> val (add 10) (mul 2) str

; conditionally render val1 
if cond val1

; conditionally render val1 or val2
if cond val1 val2

; Block start and end.
{#for person @@[:persons]}
Hi {person :name}, nice to meet you!
{#end}

; Blocks that kills any content of the same line.
dead text {##for person @@[:persons]} dead text
Hi {person :name}, nice to meet you!
dead here {##end} dead here


; binds symbols to value in content.
#let symb1 val1, symb2 val2 ...

; Symbols which are bound to a map or an array become an n-arity selector function.
{##let persons @@[:persons]}
Hi {persons 0 :name}, nice to meet you!
The 3 next persons are: {-> (persons [1 2 3] :name) (join ", ")}.
the rest of persons are: {-> (persons (range 4) :name) (join ", ")}.
{##end}

; iteration of content with cartesian product bindings.
#for symb1 coll1, symb2 coll2 ...

; conditionally render contents
#if cond
#elif cond
#else
```
