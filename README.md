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
## Set and delete a left paren reader
```lisp
(illusion:set-paren-reader name predicate reader)
```

Use a `SET-PAREN-READER` to add or change a left paren reader. `NAME` is a keyword to identify and you can delete it by `(DELETE-PAREN-READER NAME)`. `PREDICATE` is a function `INDICATOR -> BOOLEAN`. Indicator is the first element of every list. It's not necessarilly a symbol and first element of a list literal, e.g. `a` in `(a b)` is also indicator. So we must carefully check the condition that indicator satisfies in `PREDICATE`. And at last, `READER` is the function `(STREAM INDICATOR) -> OBJECT` that called when `(PREDICATE INDICATOR)` satisfied. Current position of input `STREAM` is just after read `INDICATOR`.


## Examples
### Temporarilly change to preserve case reader after specific indicator
The first example, assume we want to write a `DEFINE-CLI` which take command line specs and produce a command line argument parser. The command line option is usually case sensitive, so this won't work:
```lisp
(define-cli :main
    (v version "Display version of this program")
    (V verbose "Set verbose level"))
```
`v` and `V` will both read to `V`. We can use `"v"`, `#\v`, `\v` or `|v|`, but each one is more verbose. Or we can `(setf (readtable-case *readtable*) :preserve)`, but this force us to use upcase symbols for all cl symbols. What if the reader auto turns on preserve case after encounter `DEFINE-CLI` indicator? We can define it as:
```lisp
(set-paren-reader :define-cli
                  (lambda (i)
                    (eql i 'stub-cli:define-cli))
                  (lambda (stream indicator)
                    (cons 'stub-cli:define-cli
                          (cons (read stream)
                                (with-reader-case :preserve
                                    (cl-read-list stream))))))
```
A few note about this left paren reader:
- To compare with a symbol, must given the symbol with its package name like `STUB-CLI:DEFINE-CLI`
- The reader (third parameter of `SET-PAREN-READER` should return newly cons list. Avoid using `'` or backquote. Because sometimes they create lists with shared structure and cause strange behavior.
- `ILLUSION:WITH-READER-CASE` is a trivial but handy utility, that executing the body with `(READTABLE-CASE *READTABLE*)` bind to one of `:UPCASE`, `:DOWNCASW`, `:PRESERVE` or `:INVERSE`, and unwind to previous `(READTABLE-CASE *READTABLE*)` setting after leave it.

This only saving a little effort when define cli, but similar techiques can be helpful in accessing case sensitive foreign languages. For example, inline calling a JavaScript method as that in ClojureScript and inline calling a Qt method as if in C++.

### Inline calling CommonQt methods
Calling a CommonQt method need a `#_` reader macro:
```lisp
(#_setBrush painter "brush name")
```
Using [https://github.com/commonqt/commonqt](CommonQt) methods a lot is not very pleasant because of many `#_`. If we're doing GUI programming with CommonQt, usually it make sense to have a whole package dedicated to ui definition and event handling. With the following left paren reader, we can use CommonQt methods as if using Common Lisp functions while let Common Lisp's package system and illusion do the symbol isolation:
```lisp
(set-paren-reader :commonqt
                  #'qt-symbol-p
                  (lambda (stream indicator)
                    (list* 'optimized-call t (read stream) (symbol-name indicator)
                           (cl-read-list stream))))
```

Here `(optimized-call t obj "methodName" arg1 arg2)` is how CommonQt call Qt Method `(#_methodName obj arg1 arg2)` and after this `SET-PAREN-READER` we can simply use `(|methodName obj arg1 arg2)`. Even better, we can use `(ILLUSION:SET-INDICATOR-MODE :PRESERVE-CASE)` then just `(methodName obj arg1 arg2)`.
In this indicator mode, it will first try the preserve case symbol and check if it satisfies any left paren reader predicate. If none, indicator will fallback to upcase, so all existing Common Lisp and user package symbols still works. In rare case if you have lower and mixed case symbol as function/macro names, try to isolate them with the scope that using CommonQt.
### CSS id and class attached to html element creation function name
In [https://github.com/ailisp/flute](flute) html generation library, HTML elements are defined with same name functions. `(div ...)` will create a div element. It's almost shortest possible way to generate html in Common Lisp, but with illusion, we can support haml and hiccup style id/class attached to function names like `(div#my-div.class1.class2 ...)`. To keep example short, we only process id here and writing this left paren reader for a sub-html package, assume stub-html package has `DIV` exported:
```
(set-paren-reader :html
                  (lambda (i)
                    (when (symbolp i)
                      (let ((name (symbol-name i)))
                        (when (find #\# name)
                          (let ((name-and-id (split-sequence #\# name)))
                            (multiple-value-bind (symbol access) (find-symbol (first name-and-id) :stub-html)
                              (eql access :external)))))))
                  (lambda (stream indicator)
                    (let ((name-and-id (split-sequence #\# (symbol-name indicator))))
                      (list* (find-symbol (first name-and-id) :stub-html)
                             :id (string-downcase (second name-and-id))
                             (cl-read-list stream)))))
```
## Set indicator mode
As showed in the CommonQt example, illusion support `SET-INDICATOR-MODE`. Currently `:STANDARD` (default), `:PRESERVE-CASE` and `(INDICATOR-READER . INDICATOR-FALLBACK)` is supported. `INDICATOR-READER` is a function take a stream as only required argument and return the indicator it reads. `INDICATOR-FALLBACK` is a function called when indicator not satisfied any left paren reader and take indicator as only argument, returns the object that `CL:READ` would return when reading that indicator.

# Motivation

Illusion will obviously lead to more obscure code. It won't slow down the generated program since it all happens at read time. But if carefully used, the syntax can be further simplified and gives an illusion of having a more versatile ability with using plain parens. The example usages above are real usage in [flute](https://github.com/ailisp/flute) for html generation and [lispy-cli](https://github.com/ailisp/lispy-cli). Hope illusion also help construct easier usage of your library!

# License

Licensed under the MIT License.
Copyright (c) 2018, Bo Yao. All rights reserved.
