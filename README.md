# hopen 『活板』

A simple, modern, flexible and portable template engine for Clojure environments.

# Get started

The API is very simple.

```clojure
(render "Hello {[:name]}" {:name "john"})
=> "Hello john"

(parse-template "Hello {[:name]}")
=> [[:hopen/text "Hello "]
    [:env [:name]]]

```

# Default Template Syntax

This syntax is designed to be familiar to Clojure developers.

```clojure
; opening and closing
(...)
(:name)
(#get [:name])

; replace opening and closing by something else
#() { }
#() < >
#() % %
#() {{ }}
#() {[ ]}
#() %[ ]%

; replace opening and closing back to default
#() ( )

; an immediate value, can be any clojure value except functions.
#val 42
#v 42

; data at provided path in the root context of the document.
#root [:person 0 :name]
#r [:person 0 :name]

; data at provided relative path in the current context.
#get [:name]
#g [:name]
[:name] ; only at root level in expression
:name   ; only at root level in expression

; binds symbols to value in content.
#let symb1 val1, symb2 val2 ...

; iteration of content with cartesian product bindings.
#for symb1 coll1, symb2 coll2 ...

; conditionally render the content
#if cond

; conditionally render val1 
#if cond val1

; conditionally render val1 or val2
#if cond val1 val2

; thread operation
#-> val (#add 10) (#mul 2) #str
```
