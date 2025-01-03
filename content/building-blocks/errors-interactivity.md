+++
title = "Errors and interactivity"
weight = 100
+++

In all frameworks, we can choose the level of interactivity.

The web framework can be interactive in different degrees. What should
it do when an error happens?
- not being interactive: it returns a 404 page and prints the error output on the REPL
- not interactive but developper friendly: it can show the lisp error message on the HTML page,
- as well as the full Lisp backtrace
- it can let the developper have the interactive debugger: in that
  case the framework doesn't catch the error, it lets it through, and
  we the developper deal with it as in a normal Slime session.

We see this by default:

![](https://lisp-journey.gitlab.io/images/hunchentoot-error.png)

but we can also show backtraces and augment the data.

### Hunchentoot

The global variables to set are
- `*show-lisp-errors-p*`, nil by default
- `*show-lisp-backtraces-p*`, t by default (but the backtrace won't be shown if the previous setting is nil)
- `*catch-errors-p*`, t by default, to set to `nil` to get the interactive debugger.

We can see the backtrace on our error page:

```lisp
(setf hunchentoot:*show-lisp-errors-p* t)
```

![](https://lisp-journey.gitlab.io/images/hunchentoot-show-error.png)


### Enhancing the backtrace in the browser

We can also the library [hunchentoot-errors](https://lisp-journey.gitlab.io/images/hunchentoot-error-with-request.png) to augment the data we see in the stacktrace:

- show the **current request** (URI, headers…)
- show the **current session** (everything you stored in the session)


![](https://lisp-journey.gitlab.io/images/hunchentoot-error-with-request.png)


## Clack errors

When you use the Clack web server, you can use Clack errors, which can also show the backtrace and the session, with a colourful output:

![](https://vindarel.github.io/cl-cookbook/assets/clack-errors.png)

### Hunchentoot's conditions

Hunchentoot defines condition classes:

- `hunchentoot-warning`, the superclass for all warnings
- `hunchentoot-error`, the superclass for errors
- `parameter-error`

See the (light) documentation: [https://edicl.github.io/hunchentoot/#conditions](https://edicl.github.io/hunchentoot/#conditions).

## References

- [https://lisp-journey.gitlab.io/blog/common-lisp-on-the-web-enrich-your-stacktrace-with-request-and-session-data/](https://lisp-journey.gitlab.io/blog/common-lisp-on-the-web-enrich-your-stacktrace-with-request-and-session-data/)
