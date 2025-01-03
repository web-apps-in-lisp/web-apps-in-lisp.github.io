+++
title = "the first form"
weight = 200
+++

Our root page shows a list of products. We want to do better: provide
a search form.

Do you find HTML forms boring, very boring tech? There is no escaping
though, you must know the basics. Go [read MDN](https://developer.mozilla.org/en-US/docs/Learn_web_development/Extensions/Forms/How_to_structure_a_web_form). It's only later that you'll have the right to find and use libraries to do them for you.

## A search form

Here's a search form:

```html
(defparameter *template-root* "
<form action=\"/\" method=\"GET\">
  <div>
    <label for=\"query\">What do you search for?</label>
    <input name=\"query\" id=\"query\" placeholder=\"Search…\" />
  </div>
  <div>
    <button>Search</button>
  </div>
</form>
")
```

The important elements are the following:

- action points to `/`: we will re-use our root endpoint to handle the search and show search results. This could be another route.
- it is very important to use the `name="query"` attribute, because this is the name of the URL parameter that will be added when you press the validation button.
- we still had to escape the quotes, but don't miss our previous tip about that.

If you type "two" in the input field and validate the form, a GET
request is sent to `/` with the full URL being: `/?query=two`.

As a consequence, your route should have a parameter named `query`.

{{% notice info %}}

It's best to declare your parameters, but rest assured that
Hunchentoot allows you to get the value of any URL parameter of the
current web request with the function `hunchentoot:parameter`.

{{% /notice %}}


## Add URL parameters to the route

We are re-using our root route.

So it now looks like:

```lisp
(easy-routes:defroute root ("/") (query)
```

Can we proove it works? Let's show the query string in the template.

We add this in the root template:

```html
{% if query %}
<div> query is: {{ query }} </div>
{% endif %}
```

and we pass a new argument to render the template:

```lisp
(easy-routes:defroute root ("/") (query)
  (render *template-root*
          :products (products)
          :query query))
```

Go to [http://localhost:8899/?query=two](http://localhost:8899/?query=two) and you should see:

```
query is: two
What do you search for? [ Search…  ]
[Search]
```

It's time we do the most interesting part of the app! (I prefer back-end to front-end)

## Searching our products

We have a web form that gives us a search string.

We have a database with hundreds of thousands of products.

All we need is to efficiently filter our data and display a list of results.

But web things first, so we'll first write a very simple prototype.

Our products are named "product nb …" and we will search for the
`query` string in their names.

We can make things a lil' bit more interesting with this small change:

```lisp

(defun get-product (n)
  (list n (format nil "Product nb ~r" n) 9.99))
```

Did you notice? `format … "~r"` instead of `"~a"`, for the Radix directive. It prints numbers in english.

```lisp
MYPROJECT> (products)
((0 "Product nb zero" 9.99) (1 "Product nb one" 9.99) (2 "Product nb two" 9.99)
 (3 "Product nb three" 9.99) (4 "Product nb four" 9.99))
```

Now we will simply iterate on this list of products and collect the
ones that contain a query string. Our search function will have this signature:

```lisp
(defun search-products (products query)
```

How do you do it?

I did this:

```lisp
(defun search-products (products query)
  (loop for product in products
        if (search query (second product) :test #'equalp)
          collect product))
```

Where `search` works on two sequences, and for strings don't forget to
specify the `:test` function, and here `equal` and `string-equal` would work but `equalp` is string insensitive.

Usage:

```lisp
MYPROJECT> (search-products (products) "on")
((1 "Product nb one" 9.99))
```

We could use `str:containsp` with an optional argument `:ignore-case t`.

We could use `remove-if` and a lambda function, or define a short helper function.

Anyways. We did some hard work. Let's show it to the internet!

## Display results

We must:

- edit our route, to search for products
- edit our template, to show a list of results

Can you do it?

I did this for the template:

```html
(defparameter *template-root* "

<form action=\"/\" method=\"GET\">
  <div>
    <label for=\"query\">What do you search for?</label>
    <input name=\"query\" id=\"query\" placeholder=\"Search…\" />
  </div>
  <div>
    <button>Search</button>
  </div>
</form>

{% if query %}
<div> query is: {{ query }} </div>

<ul>
  {% for product in results %}
    <li>
      <a href=\"/product/{{ product.0 }}\">{{ product.1 }} - {{ product.2 }}</a>
    </li>
  {% endfor %}
</ul>
{% endif %}
")
```

I used a `results` variable, which is a list of product objects.

I did this for the route:

```lisp
(easy-routes:defroute root ("/") (query)
  (render *template-root*
          :results (search-products (products) query)
          :query query))
```

Go to [http://localhost:8899/?query=two](http://localhost:8899/?query=two) and you should see:

```
What do you search for? [Search… ]
[Search]

query is: two

* Product nb two - 9.99
```

(do you like my screencast? It's made from Emacs' eww :p )

Can you feel that the power of the web is at your fingertips?

Before we dive into many more topics, I'd like to ensure we know how
to run our app, from outside of the comfort of our editor.


## Full code

Our app now look like this:

```lisp
(in-package :myproject)

;;; Parameters.
(defparameter *port* 8899 "The application port.")

;;; Internal variables.
(defvar *server* nil
  "Server instance (Hunchentoot acceptor).")

;;; Templates.
(defparameter *template-root* "
<form action=\"/\" method=\"GET\">
  <div>
    <label for=\"query\">What do you search for?</label>
    <input name=\"query\" id=\"query\" placeholder=\"Search…\" />
  </div>
  <div>
    <button>Search</button>
  </div>
</form>

{% if query %}
<div> query is: {{ query }} </div>

<ul>
  {% for product in results %}
    <li>
      <a href=\"/product/{{ product.0 }}\">{{ product.1 }} - {{ product.2 }}</a>
    </li>
  {% endfor %}
</ul>
{% endif %}
")

(defparameter *template-product* "
<body>
     {{ product }}

{% if debug %} debug info! {% endif %}
</body>
")

(defun render (template &rest args)
  (apply
   #'djula:render-template*
   (djula:compile-string template)
   nil
   args))

;;; Models.
(defun get-product (n)
  (list n (format nil "Product nb ~r" n) 9.99))

(defun products (&optional (n 5))
  (loop for i from 0 below n
        collect (get-product i)))

(defun search-products (products query)
  (loop for product in products
        if (search query (second product) :test #'equalp)
          collect product))

;;; Routes.
(easy-routes:defroute root ("/") (query)
  (render *template-root*
          :results (search-products (products) query)
          :query query))

(easy-routes:defroute product-route ("/product/:n") (&get debug &path (n 'integer))
  (render *template-product*
          :product (get-product n)
          :debug debug))


(defun start-server (&key (port *port*))
  (format t "~&Starting the web server on port ~a~&" port)
  (force-output)
  (setf *server* (make-instance 'easy-routes:easy-routes-acceptor
                                :port (or port *port*)))
  (hunchentoot:start *server*))
```

Do you also clearly see 3 different components in this app? Templates, models, routes.
