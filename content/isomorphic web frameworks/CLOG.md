+++
title = "CLOG"
weight = 200
+++

[CLOG, the Common Lisp Omnificent
GUI](https://github.com/rabbibotton/clog/), follows a GUI paradigm for
the web platform. You don't write nested `<div>` tags, but you place
elements on the page.  It sends changes to the page you are working on
through websockets, as you add or edit functionalities. We can see
changes in real time. For example, change a colour:

```lisp
CLOG-USER> (setf (background-color *body*) :red)
```
and BAM, it's red.

We can say the CLOG experience is mindblowing.

With CLOG, you can easily create games (there is a Snake demo),
multiplayer applications (there is a chat demo)… all this by doing
everything in the backend, in Common Lisp, with a lot of interactivity
under the fingertips.

Moreover, its API is stable. The author used a similar product built
in Ada professionally for a decade, and transitioned to CLOG in Common
Lisp.

So, how can we build an interactive app with CLOG?

We will build a search form that triggers a search to the back-end on
key presses, and displays results to users as they type.

We do so without writing any JavaScript.

![](/clog-search.gif)

Before we do so, we'll create a list of dummy products, so than we have something to search for.

## Models

Let's create a package for this new app. I'll "use" functions and
macros provided by the :clog package.

```lisp
(uiop:define-package :clog-search
    (:use :cl :clog))

(in-package :clog-search)
```

Please Quickload those libraries:

```lisp
(ql:quickload '(:clog :str))
```

Now let's create our products. We quickly define a class containing an ID, a title and a price.

```lisp
(defclass product ()
  ((id :initarg :id :accessor product-id :type integer
       :documentation "Unique ID")
   (title :initarg :title :accessor product-title :type string)
   (price :initarg :price :accessor product-price :type integer)))

(defvar *product-id* 1
  "Counter to increment our unique product ID.")

(defparameter *products* '() "A list of products.")
```

We are going to create random testing products, so let's have a couple helpers to create random titles and prices.

```lisp
(defun random-price ()
  "Return an integer between 1 and 10.000 (price is expressed in cents)."
  (1+ (random 9999)))

(defparameter *title-part-1* (list "pretty" "little" "awesome" "white" "blue"))

(defparameter *title-part-2* (list "book" "car" "laptop" "travel" "screwdiver"))

(defun random-title ()
  (let ((index (random (length *title-part-1*)))
        (index-2 (random (length *title-part-2*))))
    (format nil "~a ~a" (elt *title-part-1* index) (elt *title-part-2* index-2))))
```
try it out:

```lisp
(random-title)
```

We get titles like "white book", "little car", etc.

Now, for testing purposes, we create 100 dummy product instances:

```lisp
(defun gen-test-products (&optional (nb 100))
  (dotimes (i nb)
    (push (make-instance 'product
                         :id (incf *product-id*)
                         :title (random-title)
                         :price (random-price))
          *products*))
    *products*)

(defun reset-test-products ()
  (setf *products* nil))
```

Try it and we get:


```lisp

*products*
(#<PRODUCT {1005B29363}> #<PRODUCT {1005B29113}> #<PRODUCT {1005B28EC3}>
 #<PRODUCT {1005B28C73}> #<PRODUCT {1005B28A23}> #<PRODUCT {1005B287D3}>
 …)
```

Implement the `print-object` method if you want nice-looking product
literals. We are not doing it yet, see the Cookbook.

Now let's display the products in the browser.

We create a couple more utility functions.

`print-product` prints a product to a stream (standard output, a new
string, any other stream). We will use it to show products in the
browser. You would add HTML markup here.

```lisp
(defun print-product (it &optional (stream nil))
  "Print a product title and price on STREAM (return a new string by default)."
  (format stream "~a - ~f~&"
          (str:fit 20 (product-title it))
          (/ (product-price it) 100)))

(defun print-products (products)
  "Return a list of products as a string (dummy, for tests purposes)."
  (with-output-to-string (s)
    (format s "Products:~&")
    (dolist (it products)
      (print-product it s))))
```

Try them:

```lisp

CL-USER> (print-products *products*)

"Products:
pretty car           -   22.26
awesome travel       -   13.87
little screwdiver    -   35.6
white laptop         -   6.08
little book          -   27.57
white laptop         -   42.63
blue travel          -   93.8
blue car             -   29.99
pretty car           -   38.95
little screwdiver    -   46.99
…
```

Now we need a function to filter our list of products from a given search query:

```lisp
(defun search-products (query &optional (products *products*))
  "Search for QUERY in the products' title.
  This would be a DB call."
  (loop for product in products
     when (str:containsp (str:downcase query) (str:downcase (product-title product)))
     collect product))
```

We use `str:containsp`, we could have used `search seq1 seq2`, with a
`:test #'equalp` for case insensitivity.

We are ready to build a little web UI.


## CLOG first steps

The very first steps you can do to grasp CLOG's interactive fun is to
make changes to a browser window while on the CLOG REPL.

```lisp
;; We suppose you did
;; (ql:quickload "clog")
CL-USER> (in-package clog-user)
CLOG-USER> (clog-repl)
NOTICE: Running in debug mode. Debugger will be invoked on errors.
  Specify ':debug nil' to turn it off on remote environments.
Hunchentoot server is started.
Listening on 0.0.0.0:8080.
HTTP listening on    : 0.0.0.0:8080
HTML root            : /home/vince/quicklisp/dists/quicklisp/software/clog-20241012-git/./static-files/
SSL                  : no
SSL Key File         : NIL
SSL Cert File        : NIL
Long poll first      : no
Boot function added  : no
Boot html source use : compiled version, when no file
Boot js source use   : compiled version
Boot file for path / : /debug.html
"Use clog-user:*body* to access the clog-repl window."
New connection id - c25394291973074d81962563fd0affc0 - #<SERVER {100C900E53}>

[this opens your browser]

CLOG-USER> (setf (background-color *body*) "red")
CLOG-USER> (create-div *body* :content "Hello World!")
```

And voilà. A browser window was opened for you, you set its background
to red, you displayed text and you saw the changes as you executed each step.

You will find many demos here: https://github.com/rabbibotton/clog/tree/main/tutorial

You can run each demo with `(clog:run-tutorial 1)` (by their number id).

### Creating elements

For the following, I invite you to have a look at [CLOG's common elements](https://rabbibotton.github.io/clog/clog-manual.html#CLOG:@CLOG-ELEMENT-COMMON%20MGL-PAX:SECTION). For instance:

```
create-a to create a <a>
link
target
create-button
create-div
create-dialog
close-dialog
…
create-table
…
```

Typically, to create a `div` on a DOM element, we use `create-div`.

The first thing we want to start our CLOG app is the `initialize` function. Its signature:

```lisp

initialize (on-new-window-handler &key (host 0.0.0.0) (port 8080) (server hunchentoot)
 (extended-routing nil) (long-poll-first nil) (boot-file /boot.html)
 (boot-function nil) (static-boot-html nil) (static-boot-js nil)
 (static-root (merge-pathnames ./static-files/ (system-source-directory clog))))

Inititalize CLOG on a socket using HOST and PORT to serve BOOT-FILE
as the default route to establish web-socket connections and static
files located at STATIC-ROOT. […]
```

The following calls our `add-products` (which we define just below)
function with a `body` (a CLOG object) as argument.

```lisp
(defun start-tutorial ()
  "Start tutorial."
  (initialize 'add-products)
  (open-browser))
```

You'll need to run the `(start-tutorial)` at some point. We'll define
`add-products` now.

OK so what do we want to do? We want to create a search input field,
and to display our products below it. When the user types something, we
want to *immediately* filter the products, and re-display them.

A first version where we only display products would be this:

```lisp
(defun add-products (body)
  (let* ((result-div (create-div body :content "")))
    (display-products result-div (subseq *products* 0 10))))
```

We create a `<div>`, we keep a reference to it as `result-div`, and we
pass it as a parameter to the function `display-products` we define next.

This function will create another div for each product to display. It
creates those children in the `result-div`.

```lisp
(defun display-products (parent products)
  "Display these products in the page.
  Create a div per product, with a string to present the product.
  We don't create nice-looking Bulma product cards here."
  (dolist (it products)
      (create-div parent :content
                  (format nil "~a - ~a"
                          (product-id it)
                          (print-product it)))))

```

You can run `(start-tutorial)` and try. You should see a list of 10 products.


### Adding interactivity: key up event

Now we want to handle the interactivity. The event to watch is the key up event. In CLOG, we have the `set-on-key-up` method. It takes: a CLOG object (the DOM object it watches for events) and a handler function. This function takes two arguments: the parent CLOG object and the event.

We augment and replace the `add-products` function with the one below where:
- we create the search input
  - with `create-form-element`
- and we listen to the "key-up" event.
  - each event executes the lambda function
  - this function clears the current display and re-displays results by calling our `handle-filter-product`.

```lisp
(defun add-products (body)
  "Create the search input and a div to contain the products.
  Bind the key-up event of the input field to our filter function."
  (let* ((form (create-form body))
         (input (create-form-element form :input :name "query"
                                     :label
				     (create-label form :content "Filter product: ")))
         (result-div (create-div body :content "" )))

    (set-on-key-up input
                   (lambda (obj event)
                     (format t ":key-up, value: ~a~&" (value obj)) ; logging
                     (setf (text result-div) "") ; this is how we erase the current content.
                     (handle-filter-product result-div obj event)))

    ;; Initial content.
    (display-products result-div (subseq *products* 0 10))))
```

Below is the function that filters the results with our input and redisplays everything.
The value of the search input was read with `(value obj)`.

We wait for at least 3 characters in the search input before firing a new query.

```lisp
(defun handle-filter-product (div obj event)
  "Search and redisplay products."
  ;; TODO: wait a little latency
  (declare (ignorable event))
  (let ((query (value obj)))
    (if (> (length query) 2)
        (display-products div (clog-search::search-products query))
        (print "waiting for more input"))))
```

It works \o/

![](/clog-search.gif)

There are some caveats that need to be worked on:

- if you type a search query of 4 letters quickly, our handler waits for an input of at least 2 characters, but it will be fired 2 other times. That will probably fix the blickering.


And, as you noticed:

- we didn't copy-paste a nice looking HTML template, so we have a bit of work with that :/


This was only an introduction. As we said, CLOG is well suited for a
wide range of applications.

## Full code

```lisp
;; (ql:quickload '(:clog :str))

(uiop:define-package :clog-search
    (:use :cl :clog))

(in-package :clog-search)


;;; Models.

(defclass product ()
  ((id :initarg :id :accessor product-id :type integer
       :documentation "Unique ID")
   (title :initarg :title :accessor product-title :type string)
   (price :initarg :price :accessor product-price :type integer)))

(defvar *product-id* 1
  "Stupid counter to increment our unique product ID.
  Normally this is given by a DB.")

(defparameter *products* '() "A list of products.")

(defun random-price ()
  "Return an integer between 1 and 10.000 (price is expressed in cents)."
  (1+ (random 9999)))

(defparameter *title-part-1* (list "pretty" "little" "awesome" "white" "blue"))

(defparameter *title-part-2* (list "book" "car" "laptop" "travel" "screwdiver"))

(defun random-title ()
  (let ((index (random (length *title-part-1*)))
        (index-2 (random (length *title-part-2*))))
    (format nil "~a ~a" (elt *title-part-1* index) (elt *title-part-2* index-2))))

(defun gen-test-products (&optional (nb 100))
  (dotimes (i nb)
    (push (make-instance 'product
                         :id (incf *product-id*)
                         :title (random-title)
                         :price (random-price))
          *products*))
  *products*)

(defun reset-test-products ()
  (setf *products* nil))

(defun print-product (it &optional (stream nil))
  "Print a product title and price on STREAM (return a new string by default)."
  (format stream "~a - ~f~&"
          (str:fit 20 (product-title it))
          (/ (product-price it) 100)))

(defun print-products (products)
  "Return a list of products as a string (dummy, for tests purposes)."
  (with-output-to-string (s)
    (format s "Products:~&")
    (dolist (it products)
      (print-product it s))))

(defun search-products (query &optional (products *products*))
  "Search for QUERY in the products' title.
  This would be a DB call."
  (loop for product in products
     when (str:containsp (str:downcase query) (str:downcase (product-title product)))
     collect product))


;;; CLOG

(defun start-tutorial ()
  "Start tutorial."
  (initialize 'add-products)
  (open-browser))


(defun add-products (body)
  "Create the search input and a div to contain the products.
  Bind the key-up event of the input field to our filter function."
  (let* ((form (create-form body))
         (input (create-form-element form :input :name "query"
                                     :label
				     (create-label form :content "Filter product: ")))
         (result-div (create-div body :content "" )))

    (set-on-key-up input
                   (lambda (obj event)
                     (format t ":key-up, value: ~a~&" (value obj)) ; logging
                     (setf (text result-div) "") ; this is how we erase the current content.
                     (handle-filter-product result-div obj event)))

    (display-products result-div *products*)))

(defun display-products (parent products)
  "Display these products in the page.
  Create a div per product, with a string to present the product.
  We don't create nice-looking Bulma product cards here."
  (dolist (it products)
      (create-div parent :content
                  (format nil "~a - ~a"
                          (product-id it)
                          (print-product it)))))

(defun handle-filter-product (div obj event)
  "Search and redisplay products."
  ;TODO: wait a little latency
  (declare (ignorable event))
  (let ((query (value obj)))
    (if (> (length query) 2)
        (display-products div (search-products query))
        (print "waiting for more input"))))
```

## References

- CLOG: https://rabbibotton.github.io/clog/clog-manual.html
