+++
title = "Templates"
weight = 30
+++


### Djula - HTML markup

[Djula](https://github.com/mmontone/djula) is a port of Python's
Django template engine to Common Lisp. It has [excellent documentation](https://mmontone.github.io/djula/djula/).

Install it if you didn't already:

~~~lisp
(ql:quickload "djula")
~~~

The Caveman framework uses it by default, but otherwise it is not difficult to
setup. We must declare where our templates live with something like:

~~~lisp
(djula:add-template-directory (asdf:system-relative-pathname "myproject" "templates/"))
~~~

and then we can declare and compile the ones we use, for example::

~~~lisp
(defparameter +base.html+ (djula:compile-template* "base.html"))
(defparameter +products.html+ (djula:compile-template* "products.html"))
~~~

{{% notice info %}}

If you get an error message when calling `add-template-directory` like this

```
The value
  #P"/home/user/…/project/src/templates/index.html"

is not of type
  STRING
from the function type declaration.
   [Condition of type TYPE-ERROR]
```

then update your Quicklisp dist or clone Djula in `~/quicklisp/local-projects/`.

{{% /notice %}}


A Djula template looks like this:

```html
{% extends "base.html" %}
{% block title %} Products page {% endblock %}
{% block content %}
  <ul>
  {% for product in products %}
    <li><a href="{{ product.id }}">{{ product.name }}</a></li>
  {% endfor %}
  </ul>
{% endblock %}
```

This template actually inherits a first one, `base.html`, which can be:

```html
<html>
 <head>
  <meta charset="utf-8">
  <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/@picocss/pico@2/css/pico.min.css">

  <title>{% block title %} My Lisp app {% endblock %}</title>

 </head>
 <body>
  <div class="container">
   {% block content %}  {% endblock %}
  </div>
 </body>
</html>
```

This base template defines two blocks: one for the page title, one for
the page content. A template that wants to inherit this base template
will use `{% extends "base.html" %}` and replace each blocks with `{% block
content %} … {‰ endblock %}`.


At last, to render the template, call `djula:render-template*` inside a route.

~~~lisp
(easy-routes:defroute root ("/" :method :get) ()
  (djula:render-template* +products.html+
                          nil
                          :products (products)
~~~

Djula is, along with its companion
[access](https://github.com/AccelerationNet/access/) library, one of
the most downloaded libraries of Quicklisp.

#### Djula filters

Filters are only waiting for the developers to define their own, so we should have a work about them.

They allow to modify how a variable is displayed. Djula comes with
a good set of built-in filters and they are [well documented](https://mmontone.github.io/djula/djula/Filters.html#Filters). They are not to be confused with [tags](https://mmontone.github.io/djula/djula/Tags.html#Tags).

They look like this: `{{ var | lower }}`, where `lower` is an
existing filter, which renders the text into lowercase.

Filters sometimes take arguments. For example: `{{ var | add:2 }}` calls
the `add` filter with arguments `var` and 2.

Moreover, it is very easy to define custom filters. All we have to do
is to use the `def-filter` macro, which takes the variable as first
argument, and which can take more optional arguments.

Its general form is:

~~~lisp
(def-filter :myfilter-name (var arg) ;; arg is optional
   (body))
~~~

and it is used like this: `{{ var | myfilter-name }}`.

Here's how the `add` filter is defined:

~~~lisp
(def-filter :add (it n)
  (+ it (parse-integer n)))
~~~

Once you have written a custom filter, you can use it right away
throughout the application.

Filters are very handy to move non-trivial formatting or logic from the
templates to the backend.


### Spinneret - lispy templates

[Spinneret](https://github.com/ruricolist/spinneret) is a "lispy"
HTML5 generator. It looks like this:

~~~lisp
(with-page (:title "Home page")
  (:header
   (:h1 "Home page"))
  (:section
   ("~A, here is *your* shopping list: " *user-name*)
   (:ol (dolist (item *shopping-list*)
          (:li (1+ (random 10)) item))))
  (:footer ("Last login: ~A" *last-login*)))
~~~

I find Spinneret easier to use than the more famous cl-who, but I
personnally prefer to use HTML templates.

Spinneret has nice features under it sleeves:

- it warns on invalid tags and attributes
- it can automatically number headers, given their depth
- it pretty prints html per default, with control over line breaks
- it understands embedded markdown
- it can tell where in the document a generator function is (see `get-html-tag`)
