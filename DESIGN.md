This file contains some proposals for this project's design decisions.

## Different forms of templates

If we wanted to make an analogy between this library and the JVM:
- the text-based, user-facing templates are the JVM languages,
- the "data template" format is the JVM byte code specification,
- the template renderers are the JVM runtimes.

### User facing templates

**Text-based**, they are described using one of the supported syntax, e.g.

- Selmer's syntax, inspired from [Django](https://docs.djangoproject.com/en/dev/ref/templates/builtins/).
- "Silly-j", a syntax designed to feel familiar to Clojure users.
- Any other syntax that can be transformed into the "data template" format.

### Parsed template

> "It's just data"
>
> -- Rich Hickey

Also known as the "data template", it is designed to be serialized into
EDN format, to be read and processed by programs.

This format is a standard for this library and it has a spec.

Note: In the long run, this format has the potential to become a standard
to represent parsed templates in the Clojure eco-system, so we *may* want
to provide its spec as a separate project at some point.

Example of parsed template in the EDN format:

```clojure
[;; Some immediate values.
 "hello " :foo-kw bar-symb true 3
 
 ;; Referencing data.
 (hopen/root :name)
 (hopen/ctx :name)
 (my-var :name)
 
 (get-in my-var [:persons 0 :name])
 (collect my-var [:persons [0 1 2] :name])
 
 ;; Scoped redefinition of the current context.
 (let [hopen/ctx (get-in hopen/ctx [:persons 0])]
    (hopen/ctx :name))
 
 ;; Scoped binding on an arbitrary symbol.
 (let [person (get-in hopen/ctx [:persons 0])]
    (person :name))
 
 ;; Iterations, scoped binding on cartesian products.
 (for [person (hopen/ctx :persons)]
    (person :name))
 
 ;; Conditional rendering.
 (if (hopen/ctx :something)
   (get-in hopen/ctx [:persons 0 :name])  ; then clause
   (get-in hopen/ctx [:persons 1 :name])) ; else clause
 
 ;; This works as well inline.
 (get-in hopen/ctx [:persons (if (hopen/ctx :something) 0 1) :name])
 
 ;; More conditional rendering.
 (case (hopen/ctx :chosen-one)
   0        nil
   :the-one (get-in hopen/ctx [:persons 1 :name])
   true     {:name "Trueman"}
   "else clause")
 
 ;; Helper functions.
 (inc (hopen/ctx :index))
 (upper (person :name))
 
 ;; Helper functions can also be user-defined.
 (sum-of-squares 3 4) ; {'sum-of-squares (fn [x y] (+ (* x x) (* y y)))}
 ]
```

### Renderer

This template form is optimized for rendering. How it is doing it and
how it is used depends on the implementation.

Just like the JVM runtimes, there are some which are pure interpreters and some
which optimize using Just-In-Time compilation techniques.

## Hopen's API

Something like that:

```clojure
(require '[hopen.renderer.xf :as rxf]
         '[hopen.syntax.silly-j :as silly-j])

;; Parse a template using one of the supported syntax.
(def data-template
  (silly-j/parse "Hello {@:name}, {@:n} * {@:n} = {square @:n}"))

;; Prepare a template for rendering using one of the supported renderer.
(def renderer
  (rxf/renderer data-template
                (update rxf/default-env :bindings assoc
                  'square (fn [x] (* x x)))))

;; Use the renderer to output the data into a string.
(transduce renderer str [{:name "Alice", :n 3}])
; => "Hello Alice, 3 * 3 = 9"
```
