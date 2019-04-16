# hopen 『活板』

[![Build Status](https://api.travis-ci.org/clojure-tw/hopen.svg?branch=master)](https://travis-ci.org/clojure-tw/hopen)
[![CircleCI](https://circleci.com/gh/clojure-tw/hopen/tree/master.svg?style=svg)](https://circleci.com/gh/clojure-tw/hopen/tree/master)
[![Join the chat at https://gitter.im/clojure-hopen/community](https://badges.gitter.im/clojure-hopen/community.svg)](https://gitter.im/clojure-hopen/community?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

A simple, modern, flexible and portable template engine for Clojure environments.

## Usage

```clojure
(ns myproject.core
  (:require [hopen.syntax.handlebars :as hb] ; choose a parser
            [hopen.renderer.xf :as rxf]      ; choose a renderer
            [hopen.renderer.env :as env]))

(def handlebars-template "
<div class=\"entry\">
  <h1>{{title}}</h1>
  <h2>By {{author.name}}</h2>

  <div class=\"body\">
    {{body}}
  </div>
</div>")

;; The template's assembly
(def parsed-template (hb/parse handlebars-template))

(def data
  {:title "Alice in Wonderland"
   :author {:name "Charles Lutwidge Dodgson"}
   :body "Some text ..."})

(def rendered-html
  (let [env (-> env/standard-env
                (hb/with-handlebars-env)
                (rxf/with-renderer-env))]
    (transduce (renderer parsed-template env) str [data])))
```

## Introduction

### Template as data

> "One Ring to rule them all"
> -- J. R. R. Tolkien

Hopen explicitly decouples the parsing of a template and its rendering.
It uses a plain Clojure data structure (which we call "template assembly") to represent the parsed templates.

The advantage of this approach is flexibility: The template can be parsed somewhere,
the resulting template assembly be serialized as EDN, be stored or sent over the network
somewhere else and be rendered now or later.

### Multiple parsers and renderers

As a consequence of its design, Hopen can have multiple parsers and multiple renderers.
We could make an analogy with the Java Virtual Machine.

The JVM has:
- Multiple Languages implemented on the top of the JVM.
- A data format representing programs: the byte code.
- Multiple runtimes, to interpret the byte code according to different needs.

Hopen has:
- Multiple parsers for different template syntax.
- A data format representing the templates: the template assembly.
- Multiple renderers, to display the template according to different needs.

### Flexibility

Hopen only parses templates and renders them, it does nothing else (e.g. no I/O).

The renderer currently available is a transducer that outputs *Clojure values* (not strings).
The user is free to use it the way s/he wants.

The template assembly is just a vector of Clojure values, some of them are lists of values which are
interpreted similarly to Clojure function calls.

```clojure
;; Example of template assembly
'["Hello "
  (str "M. " (get-in hopen/ctx [:person :name]))
  ", it is a pleasure to meet you."]
```

These functions are specified in an environment map which the user provides to the renderer.
Hopen provides a standard environment map which the user is free to use or not, or to modify and use.

### Portability

Hopen runs on both Clojure and Clojurescript. It is tested via Circle-CI and Travis-CI on
a wide range of environments (JVM, Chrome, Firefox and NodeJS).

## Status

The project is currently a work in progress.

Syntaxes currently supported:
- [ ] [Handlebars](https://handlebarsjs.com/) - WIP, 90% completed.
- [ ] Silly-J, a syntax similar to Clojure - WIP.

Renderers currently supported:
- [x] A renderer as a transducer, outputs Clojure values.
- [ ] A renderer generated via macro from a template assembly - WIP.

## F.A.Q.

### Why not directly generate Clojure code?

> The format of the template assembly is very similar to Clojure,
> why the parsers are not directly generating Clojure code?

The goals of the template assembly are:
- to provide enough functionality for rendering templates,
- to be easy to be interpreted and processed as data,
- to be sandbagged, for security reasons.

If the output was some Clojure code without any restriction, it would be a lot harder to process it as data.
For this reason, the template assembly is only a small DSL, not a Turing-complete programming language.

It happens to look like Clojure because it makes it simpler to read, understand and process.

## Contributing

Basically:
- If you find a bug, please open an issue or submit a PR.
- If you want a new feature, open an issue so it can be discussed.

Important notes if you want to post a PR:
- PR about new features may not be accepted easily, the best is to discuss them with the maintainers first.
- Clean your commits before asking for a review, squash some of them when it makes sense.
- If your PR is modifying multiple parts of the project, splitting your PR may be requested.
- Do not over comment your implementation, make it easy to read and explicit instead.
- Decouple your functions when it makes sense to do so.
- Do not wait until the very last commit for asking review and feedback.
- We **only** merge source code which is easy to read and which is tested.
- [Vincent](https://github.com/green-coder) has a high standard for source code readability and quality.
- Discuss with the maintainers if you want to add extra dependencies or change the project's environment.
- Some PR may never get merged.

The maintainers are:
- [Hylisd](https://github.com/hylisd)
- [Vincent](https://github.com/green-coder)

## License

The use and distribution terms for this software are covered by the
[Eclipse Public License 1.0](https://opensource.org/licenses/eclipse-1.0),
which can be found in the file [epl-v10.md](epl-v10.md) at the root of
this project.

By using this software in any fashion, you are agreeing to be bound by the
terms of this license. You must not remove this notice, or any other, from
this software.
