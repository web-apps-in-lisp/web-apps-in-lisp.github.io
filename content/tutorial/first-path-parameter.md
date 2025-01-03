+++
title = "Part 1: the first path parameter"
weight = 20
+++

So far we have 1 route that displays all our products.

We will make each product line clickable, to open a new page, that will show more details.

Each product detail will be available on the URL `/product/n` where `n` is the product ID.

## Add links

To begin with, let's add links to the list of products:

```lisp
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
```

Carefully observe that, in the `href`, we had to escape the quotes :/
This is the shortcoming of defining Djula templates as strings in
.lisp files. It's best to move them to their own directory and own
files.


{{% notice info %}}

You can also use a [triple quotes reader](https://github.com/smithzvk/pythonic-string-reader):

```lisp
;; after (ql:quickload "pythonic-string-reader")
(pythonic-string-reader:enable-pythonic-string-syntax)

(defparameter *template-root* """<a href="/yes"></a>""")
```

{{% /notice %}}

However now that you copy-pasted, re-compile the variable, refresh the
page, and click on a link: you should get a "Not Found" error.

We need to create a new route to handle the `/products/n` URL.

## Path parameter

The `n` is a path parameter. It is handled by easy-routes with this syntax:

```lisp
(easy-routes:defroute product-route ("/product/:n") ()
  (format nil "parameter is: ~a" n))
```

Go again to [http://localhost:8899/product/0](http://localhost:8899/product/0) and you should see:

    parameter is: 0

Perfect.

Inside our route, `n` is a string. Let's proove it:

```lisp
(easy-routes:defroute product-route ("/product/:n") ()
  (format nil "parameter is: ~a and is of type: ~a" n (type-of n)))
```

you should see "parameter is: 0 and is of type: (SIMPLE-ARRAY
CHARACTER (1))", and an array of characters is a string.

We can parse a string to an integer with `parse-integer`, but look,
Hunchentoot, and by extension easy-routes, have shortcuts where we can
specify the type of each path or URL parameter. The parsing is done by
them. Let's try:

```lisp
(easy-routes:defroute product-route ("/product/:n") (&path (n 'integer))
  (format nil "parameter is: ~a and is of type: ~a" n (type-of n)))
```

look at `(&path (n 'integer))`. When I reload the page, I learn that
the `type-of` 0 and 1 is `BIT`, and that of 3 is `(INTEGER 0 4611686018427387903)`.

If I visit the page with the wrong parameter "foo": [http://localhost:8899/product/foo](http://localhost:8899/product/foo), we get `n` as NIL. Alright.

With that information, let's return a product's details.

## Show a product page

So what do we need to show a product page?

- a route for the product URL (OK)
- the product ID (OK)
- a template for the product
- get a product object from its ID (you would typically query the database here, but we won't)
- rendering the template with a given product.

Look, we are not doing any error handling and I'm not going to invent
requirements for this app. Let's get to the simplest functions! Real
use cases will come in time.

Can you come up with a template and the route?

Here are mine:

```lisp
(defparameter *template-product* "
<body>
     {{ product }}
</body>
")

(defun get-product (n)
  ;; Query the DB.
  (list n (format nil "Product nb ~a" n) 9.99))

(defun render-product (n)
  (djula:render-template*
   (djula:compile-string *template-product*)
   nil
   :product (get-product n)))

(easy-routes:defroute product-route ("/product/:n") (&path (n 'integer))
  (render-product n))
```

yeah I'm just printing the product, as a list, very simply.

However I don't like the copy-pasting between `render-product` and `render-products` so I'll fix it. Can you too?

## Small refactor

I came up with one single `render` function that takes a template as
parameter, and as many key arguments as needed, that it passes to
`djula:render-template*`.

I also added a `get-product (n)` function helper. This is the function
that is supposed to hit the database.


```lisp
(defun render (template &rest args)
  (apply
   #'djula:render-template*
   (djula:compile-string template)
   nil
   args))

(easy-routes:defroute root ("/") ()
  (render *template-root* :products (products)))

(easy-routes:defroute product-route ("/product/:n") (&path (n 'integer))
  (render *template-product* :product (get-product n)))
```

That's better. Usually all my routes have this form: name, arguments,
call to some sort of render function with a template and arguments.

I'd like to carry on with features but let's have a word about *URL parameters*.
