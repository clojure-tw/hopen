This file contains some proposals for this project's design decisions.

## Different forms of templates

### User facing templates

Text-based, described using one of the supported syntax, e.g.

- `silly-j`, Hopen's default syntax.
- Selmer's syntax.

### Parsed template
 
> "It's just data"
>
> -- Rich Hickey

It is designed to be read and processed, it could be written down
into an EDN file and be loaded back later. This format is a standard
for this library and it has a spec.

Note: In the long run, this format has the potential to become a standard
to represent parsed templates in the Clojure eco-system, so we may want
to provide its spec as a separate project.

### Compiled template

As a Clojure transducer, this template form is optimized for rendering.
It outputs a serie of printable data (e.g. numbers, strings, etc ..).

In addition to the default portable implementation of the
template compiler, some environment specific implementations
could be provided for better performance.

## Hopen's API

```clojure
(require '[hopen.core :as hopen]
         '[hopen.syntax.silly-j :as silly-j])

;; Parse a template using one of the supported syntax.
(def parsed-template
  (silly-j/parse "Hello {@:name}, {@:n} * {@:n} = {square @:n}"))

;; Compile a template for rendering.
(def template-xf
  (hopen/compile parsed-template
                 (assoc hopen/builtin-filters
                  'square (fn [x] (* x x)))))

;; Render your data into a string.
(transduce template-xf str {:name "Alice", :n 3})
; => "Hello Alice, 3 * 3 = 9"
```

# Implementation Requirements

- The filter functions should be standard clojure functions,
  they should be used in the same way by any of different
  template syntax available.
