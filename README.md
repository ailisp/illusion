# Illusion

Illusion is a library for cutomization and management of Lisp left paren reader.

# Overview

- Adding customized left paren reader macro, based on indicator (first element of list);
- Automactically use left paren reader macro when indicator satisfies user defined predicate;
- Optionally read indicator in case sensitive or customized mode and still read the rest with default reader behavior
- Delete paren left paren reader even they break reader behavior.

Why customize the reader macro of left paren? Some features are impossible without doing that, let's see a few short examples in [Usage](#Usage) section.

# Usage
## Installation and import
Before illusion available in quicklisp, clone this repo to `local-projects` or adding to `asdf:*central-registry*` and:
```lisp
(ql:quickload :illusion)
```
If you don't use other customized reader macros, just use `:illusion-readtable`:
```lisp
(named-readtables:in-readtable :illusion-readtable)
```
Otherwise, merge `:illusion-readtable` into current one, it only changes definition of `#\(` and `#\)`:
```lisp
(handler-bind ((named-readtables:reader-macro-conflict #'continue))
           (named-readtables:merge-readtables-into your-readtable :illusion-readtable))
```
## Add a left paren reader
```lisp
(illusion:set-paren-reader name predicate reader)
```

TODO

# Motivation

Illusion will obviously lead to more obscure code. But if carefully used, the syntax can be further simplified and gives an illusion of having a more versatile ability with using plain parens. The example usages above are real usage in [flute](https://github.com/ailisp/flute) for html generation and [lispy-cli](https://github.com/ailisp/lispy-cli). Hope illusion also help construct easier usage of your library!

# License

Licensed under the MIT License.
Copyright (c) 2018, Bo Yao. All rights reserved.
