+++
title = "Part 1: Getting Started"
weight = -1
+++

We will:
- define a list of products, stored in a dummy database,
- we'll have an index page with a search form,
- we'll display search results,
- and have one page per product to see it in details.

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

               ;; utils
               :local-time   ;; time
               :str          ;; strings library
               :cl-ppcre     ;; regex
               :cl-slug      ;; slugs for URIs

               ;; devel
               :log4cl       ;; logging
               )
  :components ((:module "src"  ;; a src/ subdirectory
                :components
                (
                 (:file "packages")  ;; = src/packages.lisp
                 (:file "myproject") ;; = src/myproject.lisp
                )))

  ;; To build a binary:
  :build-operation "program-op"
  :build-pathname "myproject"
  :entry-point "myproject::main"  ;; supposing a `main` function exists.

  :description "A list of products")
```

Now create a `src/` subdirectory, in which you'll create the two files mentioned in the .asd.

In `src/packages.lisp`, let's create one package for our app:

```lisp
(defpackage myproject
  (:use :cl))
```

We prefer to put the package(s) definition(s) in their own file,
because we find it easier to make the application grow.

In `src/myproject.lisp`, add this important line:

```lisp
(in-package :myproject)

;; Here our application code.
```

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

