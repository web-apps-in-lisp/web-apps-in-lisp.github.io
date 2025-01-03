+++
title = "Part 1: the first template"
weight = -1
+++

Our route only returns a string:

```lisp
(easy-routes:defroute root ("/") ()
    "hello app")
```

Can we have it return a template?

We'll use Djula HTML templates.

Usually, templates go into their `templates/` directory. But, we will
start out by defining our first template inside our .lisp file:

```html
;; scr/myproject.lisp
(defparameter *template-root* "
<title> Lisp web app </title>
<body>
  hello app
</body>
")
```

Compile this variable as you go, with `C-c C-c` (or call again `load` from any REPL).

Now come back to the `root` route. We have to change it. Instead of
"hello app", we use Djula to:

- compile our string to a Djula template: `djula:compile-string`
- render the template: `djula:render-template*`

So let's do it:

```lisp
(easy-routes:defroute root ("/" :method :get) ()
  (djula:render-template*
   (djula:compile-string *template-root*)
   nil))
```

The `nil` argument means "return a string" (as in `(format nil …)`).

Templates are most useful when they do conditional logic, inherit from
other templates and are given arguments. So, how could we render a list of products?

## Render products

The Djula iteration tag is `{% for … in … %} … {% endfor %}`. Shall we
try it to display a list of products?

As a reminder: our products can be retrieved with `(products)`.

How well do you know HTML? Nothing's fancy here. It's actually boring technology.

```html
(defparameter *template-root* "
<title> Lisp web app </title>
<body>
  <ul>
  {% for product in products %}
    <li> {{ product.1 }} - {{ product.2 }} </li>
  {% endfor %}
 </ul>
</body>
")
```

re-compile the variabe when done (`C-c C-c`).

Do you wonder what's `product.1`? Recall that each `product` is a list
of 3 three elements, so we access the element 1 of a product, which is
its title. If our product had been a hash-table with a `title` key, we
could have written `{{ product.title }}`. If the product had been an
object with a `title` slot, we could have written the same. Djula
makes it easy for us to access object properties.

How can we tell Djula to take a list of products as parameter? We
simply use `:key` arguments, like this:

```lisp
;; in the root route
(djula:render-template*
   (djula:compile-string *template-root*)
   nil
   :products (products))
```

you can compile the route again and refresh the page at [localhost:8899/](localhost:8899/).

You should see a list of products! Like so (with proper HTML bullet points):

```
- Product nb 0 - 9.99
- Product nb 1 - 9.99
- Product nb 2 - 9.99
- Product nb 3 - 9.99
- Product nb 4 - 9.99
```

Cool!

## A first refactor

We have a very cool route:

```lisp
(easy-routes:defroute root ("/" :method :get) ()
  (djula:render-template*
   (djula:compile-string *template-root*)
   nil
   :products (products)))  ;; <----- added
```

To test it and see its output, we had to re-compile it (OK), and
refresh our browser. ARGH! We can do better. It may not look necessary
now, but we are already writing business logic inside a web route. We
should extract as much logic as possible from the route. It will make
everything so much easier to test in the long run.

My point here is that we have one business rule: rendering
products. We can have a function for this:

```lisp
(defun render-products ()
  (djula:render-template*
   (djula:compile-string *template-root*)
   nil
   :products (products)))

(easy-routes:defroute root ("/" :method :get) ()
  (render-products))
```

The great benefit is that you can run `(render-products)` by itself (and
very quickly with `C-c C-y` `M-x slime-call-defun`) to test it in the
REPL, and see the HTML output.

```lisp
CL-USER> (in-package :myproject)
#<PACKAGE "MYPROJECT">
MYPROJECT> (render-products)
"
<title> Lisp web app </title>
<body>
  <ul>

    <li> Product nb 0 - 9.99 </li>

    <li> Product nb 1 - 9.99 </li>

    <li> Product nb 2 - 9.99 </li>

    <li> Product nb 3 - 9.99 </li>

    <li> Product nb 4 - 9.99 </li>

 </ul>
</body>
"
```

Now you do you, but you've been warned ;) Lisp makes small
refactorings easy, so take advantage of it. And keep in mind to
separate your application logic from the web shenanigans.

That being said, let's move on. We'll create a page to see each
product and we'll have more fun with a search form.


## Whole file

This is our `myproject.lisp` file so far:

```lisp
(in-package :myproject)

(defvar *server* nil
  "Server instance (Hunchentoot acceptor).")

(defparameter *port* 8899 "The application port.")

(defparameter *template-root* "
<title> Lisp web app </title>
<body>
  <ul>
  {% for product in products %}
    <li> {{ product.1 }} - {{ product.2 }} </li>
  {% endfor %}
 </ul>
</body>
")

(defun products (&optional (n 5))
  (loop for i from 0 below n
        collect (list i
                      (format nil "Product nb ~a" i)
                      9.99)))

(defun render-products ()
  (djula:render-template*
   (djula:compile-string *template-root*)
   nil
   :products (products)))

(easy-routes:defroute root ("/") ()
  (render-products))

(defun start-server (&key (port *port*))
  (format t "~&Starting the web server on port ~a~&" port)
  (force-output)
  (setf *server* (make-instance 'easy-routes:easy-routes-acceptor
                                :port (or port *port*)))
  (hunchentoot:start *server*))
```
