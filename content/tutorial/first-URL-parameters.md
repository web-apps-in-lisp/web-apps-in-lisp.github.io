+++
title = "the first URL parameter"
weight = 100
+++

As of now we can access URLs like these:
[http://localhost:8899/product/0](http://localhost:8899/product/0)
where 0 is a *path parameter*.

We will soon need *URL parameters* so let's add one to our routes.

We want a `debug` URL parameter that will show us more data. The URL
will accept a `?debug=t` part.


We have this product route:

```lisp
(easy-routes:defroute product-route ("/product/:n") (&path (n 'integer))
  (render *template-product* :product (get-product n)))
```

With or without the `&path` part, either is good for now, so let's remove it for clarity:

```lisp
(easy-routes:defroute product-route ("/product/:n") ()
  (render *template-product* :product (get-product n)))
```

We removed it because it's as the same place that we will define URL
parameters. Let's add one.

## GET and POST parameters

Our route with a `debug` URL parameter becomes:

```lisp
(easy-routes:defroute product-route ("/product/:n") (&get debug)
  (render *template-product* :product (get-product n)))
```

The `&get` is here to say this parameter is accepted only in GET
requests. You can have `&post`, and you can leave aside the `&get` or
`&post` for parameters that are always accepted.

Let's not parse the value of the `debug` variable: if it's present,
it's truthy, and we should display debug information.

Let's add logic to the template.

The `if` tag of Djula is of the form: `{% if %} … {% else %} … {% endif %}`.

So:

```lisp
{% if debug %} debug info! {% endif %}
```

And pass the variable to the `render` function:

```lisp
(render *template-product*
          :product (get-product n)
          :debug debug)
```

Go to
[http://localhost:8899/product/0?debug=t](http://localhost:8899/product/0?debug=t)
and you should see

     (0 Product nb 0 9.99) debug info!

Can we do something useful? In my apps, printing debug info and the
output of `describe` for some objects turned useful (grab this output
with `with-output-to-string`).

## Full code

Our app looks like this:

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
    <li>
      <a href=\"/product/{{ product.0 }}\">{{ product.1 }} - {{ product.2 }}</a>
    </li>
  {% endfor %}
 </ul>
</body>
")

(defparameter *template-product* "
<body>
     {{ product }}

{% if debug %} debug info! {% endif %}
</body>
")

(defun get-product (n)
  (list n (format nil "Product nb ~a" n) 9.99))

(defun products (&optional (n 5))
  (loop for i from 0 below n
        collect (get-product i)))

(defun render (template &rest args)
  (apply
   #'djula:render-template*
   (djula:compile-string template)
   nil
   args))

(easy-routes:defroute root ("/") ()
  (render *template-root* :products (products)))

(easy-routes:defroute product-route ("/product/:n") (&get debug &path (n 'integer))
  (render *template-product*
          :product (get-product n)
          :debug debug))


(defun start-server (&key (port *port*))
  (format t "~&Starting the web server on port ~a" port)
  (force-output)
  (setf *server* (make-instance 'easy-routes:easy-routes-acceptor
                                :port port))
  (hunchentoot:start *server*))
```

Everything is contained in one file, and we
can run everything from sources or we can build a self-contained
binary. Pretty cool!

Before we do so, we'll add a great feature: searching for products.
