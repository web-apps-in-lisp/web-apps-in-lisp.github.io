+++
title = "Error handling"
weight = 150
+++

In all frameworks, we can choose the level of interactivity. The web
framework can return a 404 page and print output on the repl, it can
catch errors and invoke the interactive lisp debugger, or it can show
the lisp backtrace on the html page.

### Hunchentoot

The global variables to set are `*catch-errors-p*`,
`*show-lisp-errors-p*` and `*show-lisp-backtraces-p*`.

Hunchentoot also defines condition classes.

See the documentation: [https://edicl.github.io/hunchentoot/#conditions](https://edicl.github.io/hunchentoot/#conditions).
