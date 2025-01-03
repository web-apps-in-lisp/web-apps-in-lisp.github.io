+++
title = "Routing"
weight = 15
+++

I prefer the easy-routes library than pure Hunchentoot to define
routes, as we did in the tutorial, so skip to its section below if you
want. However, it can only be benificial to know the built-in
Hunchentoot ways.


## Hunchentoot

### The dispatch table

The first, most basic way in Hunchentoot to create a route is to add a
URL -> function association in its "prefix dispatch" table.

To bind an existing function to a route, we create a "prefix dispatch"
that we push onto the `*dispatch-table*` list:

~~~lisp
(defun hello ()
   (format nil "Hello, it works!"))

(push
  (hunchentoot:create-prefix-dispatcher "/hello" #'hello)
  hunchentoot:*dispatch-table*)
~~~

You just created the `/hello` route.

To create a route with a regexp, we use `create-regex-dispatcher`, where
the url-as-regexp can be a string, an s-expression or a cl-ppcre scanner.

If you didn't already, create an acceptor and start the server:

~~~lisp
(defvar *server* (make-instance 'hunchentoot:easy-acceptor :port 4242))
(hunchentoot:start *server*)
~~~

and access it on [http://localhost:4242/hello.html](http://localhost:4242/hello.html).

We can see logs on the REPL:

```
127.0.0.1 - [2018-10-27 23:50:09] "get / http/1.1" 200 393 "-" "Mozilla/5.0 (X11; Linux x86_64; rv:58.0) Gecko/20100101 Firefox/58.0"
127.0.0.1 - [2018-10-27 23:50:10] "get /img/made-with-lisp-logo.jpg http/1.1" 200 12583 "http://localhost:4242/" "Mozilla/5.0 (X11; Linux x86_64; rv:58.0) Gecko/20100101 Firefox/58.0"
127.0.0.1 - [2018-10-27 23:50:10] "get /favicon.ico http/1.1" 200 1406 "-" "Mozilla/5.0 (X11; Linux x86_64; rv:58.0) Gecko/20100101 Firefox/58.0"
127.0.0.1 - [2018-10-27 23:50:19] "get /hello.html http/1.1" 200 20 "-" "Mozilla/5.0 (X11; Linux x86_64; rv:58.0) Gecko/20100101 Firefox/58.0"
```

### Easy handlers

The second way to create a route is to use the "easy handlers".

[define-easy-handler](https://edicl.github.io/hunchentoot/#define-easy-handler) allows to create a function and to bind it to an uri at once.

Its form follows

    define-easy-handler (function-name :uri <the uri> …) (parameters as a list)

where `<the uri>` can be a string or a function.

Example:

~~~lisp
(hunchentoot:define-easy-handler (say-yo :uri "/yo") (name)
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "Hey~@[ ~A~]!" name))
~~~

Visit it at [p://localhost:4242/yo](http://localhost:4242/yo) and add parameters on the url:
[http://localhost:4242/yo?name=Alice](http://localhost:4242/yo?name=Alice).

Just a thought… we didn't explicitly ask Hunchentoot to add this
route to our first acceptor of the port 4242. Let's try another acceptor (see
previous section), on port 4444: [http://localhost:4444/yo?name=Bob](http://localhost:4444/yo?name=Bob) It
works too! Our route was configured for the two "acceptors", the two web apps.
In fact, `define-easy-handler` accepts an `acceptor-names`
parameter. If you are only working one web app, that's OK. If you want to create the
route for one app only though, you'll have to use this parameter:

> acceptor-names (which is evaluated) can be a list of symbols which means that the handler will only be returned by DISPATCH-EASY-HANDLERS in acceptors which have one of these names (see ACCEPTOR-NAME). acceptor-names can also be the symbol T which means that the handler will be returned by DISPATCH-EASY-HANDLERS in every acceptor.

So, `define-easy-handler` has the following signature:

    define-easy-handler (function-name &key uri acceptor-names default-request-type) (lambda list parameters)

It also has a `default-parameter-type` which we'll use in a minute to get url parameters.

There are also keys to know for the lambda list. Please see the documentation.


## Easy-routes

[easy-routes](https://github.com/mmontone/easy-routes) is a route
handling extension on top of Hunchentoot. It provides:

- dispatch based on HTTP method (otherwise cumbersome in Hunchentoot)
- arguments extraction from the url path
- and decorators, which help for wrapping the route with more logic (ensure a user is logged in, define the request content type…).

I prefer it to raw Hunchentoot, it is simpler to use.

To use it, don't create a server with `hunchentoot:easy-acceptor` but
with `easy-routes:easy-routes-aceptor`:

~~~lisp
(setf *server* (make-instance 'easy-routes:easy-routes-acceptor))
~~~

> Note: there is also `routes-acceptor`. The difference is that `easy-routes-acceptor` iterates over Hunchentoot's `*dispatch-table*` if no route is found by `easy-routes`. That allows us, for example, to serve static content the usual way with Hunchentoot.

Then define a route like this:

~~~lisp
(easy-routes:defroute name ("/tasks/:id" :method :get) (debug &get z)
    (format nil "we want the task of id: ~a with parameters debug: ~a and z: ~a" id debug z))
~~~

You can try it on routes like `/tasks/33?debug=t&z=abc`.

Here, `:id` captures the path parameter and binds it to the `id`
variable into the route body. `debug` and `&get z` define url parameters.
With a `:post` request, we could define `&post` parameters to extract from the HTTP request
body.

These parameters can take an `:init-form` and `:parameter-type`
options as in `define-easy-handler`. The init form gives a default
value if the argument is not supplied, and the parameter type asks
Hunchentoot to convert the argument to this given type. For example,
defining an `:integer` will give you an integer and not a string (all
URL parameters are given as a string by default, but more on that on the next section).

**Decorators** are functions that are executed before the route body. They
should call the `next` parameter function to continue executing the
decoration chain and the route body finally. Examples:

~~~lisp
;; Ensure a user is logged in:
(defun @auth (next)
  (let ((*user* (hunchentoot:session-value 'user)))
    (if (not *user*)
	(hunchentoot:redirect "/login")
	(funcall next))))

;; Define content types:
(defun @html (next)
  (setf (hunchentoot:content-type*) "text/html")
  (funcall next))

(defun @json (next)
  (setf (hunchentoot:content-type*) "application/json")
  (funcall next))

;; Ensure our PostgreSQL database is connected:
(defun @db (next)
  (postmodern:with-connection *db-spec*
    (funcall next)))
~~~

I mostly use the `@auth` and `@json` decorators, which I'll demo later. See the `easy-routes` readme for more.


## Accessing GET and POST parameters

You probably have nothing to do to get the value of those parameters: if you defined your route with `easy-handler` or `easy-routes:defroute`, the URL parameters create local variables in the function body. Very much like the arguments of the `defun` macro.

However, here's how to interact more with URL parameters. In particular, we can define the default type of a parameter: they are strings by default, but we can ask to receive an integer.

### Hunchentoot URL parameters

First of all, note that we can access query parameters anytime with

~~~lisp
(hunchentoot:parameter "my-param")
~~~

It acts on the default `*request*` object which is passed to all handlers.

There is also `get-parameter` and `post-parameter`.


Earlier we saw some key parameters to `define-easy-handler`. We now
introduce `default-parameter-type`.

We defined the following handler:

~~~lisp
(hunchentoot:define-easy-handler (say-yo :uri "/yo") (name)
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "Hey ~a!" name))
~~~

The variable `name` is a string by default. Let's check it out:

~~~lisp
(hunchentoot:define-easy-handler (say-yo :uri "/yo") (name)
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "Hey ~a you are of type ~a" name (type-of name)))
~~~

Going to [http://localhost:4242/yo?name=Alice](http://localhost:4242/yo?name=Alice) returns

    Hey Alice you are of type (SIMPLE-ARRAY CHARACTER (5))

To automatically bind it to another type, we use `default-parameter-type`. It can be
one of those simple types:

* `'string` (default),
* `'integer`,
* `'character` (accepting strings of length 1 only, otherwise it is nil)
* or `'boolean`

or a compound list:

- `'(:list <type>)`
- `'(:array <type>)`
- `'(:hash-table <type>)`

where `<type>` is a simple type.
