+++
title = "Getting Started"
weight = -1
+++

In this application we will:
- define a list of products, stored in a dummy database,
- we'll have an index page with a search form,
- we'll display search results,
- and have one page per product to see it in details.

But everything starts with a project (a *system* in Lisp parlance) definition.

## Setting up the project

Let's create a regular Common Lisp project.

We could start straihgt from the REPL, but we'll need a project
definition to save our project dependencies, and other metadata. Such
a project is an ASDF file.

Create the file `myproject.asd` at the project root:

```lisp
(asdf:defsystem "myproject"
  :version "0.1"
  :author "me"
  :license "WTFPL"
  :depends-on (
               ;; web stack
               :hunchentoot  ;; web server
               :easy-routes  ;; routes facility
               :djula        ;; HTML templates
               )
  :components ((:module "src"  ;; a src/ subdirectory
                :components
                (
                 (:file "myproject") ;; = src/myproject.lisp
                )))

  :description "A list of products")
```

Now create a `src/` subdirectory, in which you'll create the one file mentioned in the .asd.

In `src/myproject.lisp`, add this:

```lisp
(defpackage myproject
  (:use :cl))

(in-package :myproject)

;; Here our application code.
```

We created a Lisp *package*, a namespace if you want, and we made sure
that the rest of our code will be written for this package. By
default, we are in the `cl-user` package.

Sometimes the package definition is done in its own `packages.lisp`
file: I like this approach for applications that grow.

we are now done with our project skeleton. Let's compile and load our
project *and its dependencies*.

Start your editor or a REPL.

Compile the .asd file with `C-c C-k` in Slime, or load it with `(load
"myproject.asd")` from any REPL.

According you have installed Quicklisp, the library manager, quickload
our system:

```lisp
CL-USER> (ql:quickload "myproject")

To load "myproject":
  Load 1 ASDF system:
    myproject
; Loading "myproject"
..................................................
[package cl-syntax]...............................
[package cl-locale.core]..........................
[package cl-locale.reader]........................
[package cl-locale.syntax]........................
[package cl-locale]...............................
[package portable-pathnames]......................
[package djula]...................................
...

("myproject")
```

We are now ready to write the application logic.

## A list of products

Our application will handle hundreds of thousands of products that it
gets from a database. For now, we just want a dummy function to return
us a list of products, so we can concentrate on the web stack.

We'll write a function that returns `n` products. Each product is a
list of three elements: the product `id`, its `name` and a `price`.

```lisp
(in-package :myproject)

(defun products (&optional (n 5))
  (loop for i from 0 below n
        collect (list i
                      (format nil "Product nb ~a" i)
                      9.99)))
```

this returns:

```
((0 "Product nb 0" 9.99) (1 "Product nb 1" 9.99) (2 "Product nb 2" 9.99)
 (3 "Product nb 3" 9.99) (4 "Product nb 4" 9.99))
```

That's not much, but that's enough to show content in a web app, which we'll do next.
