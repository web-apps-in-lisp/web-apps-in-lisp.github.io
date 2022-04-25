+++
title = "Templates"
weight = 30
+++


### Djula - HTML markup

[Djula](https://github.com/mmontone/djula) is a port of Python's
Django template engine to Common Lisp. It has [excellent documentation](https://mmontone.github.io/djula/doc/build/html/index.html).

Install it if you didn't already do it:

~~~lisp
(ql:quickload "djula")
~~~


Caveman uses it by default, but otherwise it is not difficult to
setup. We must declare where our templates live with something like:

~~~lisp
(djula:add-template-directory (asdf:system-relative-pathname "webapp" "templates/"))
~~~

and then we can declare and compile the ones we use, for example::

~~~lisp
(defparameter +base.html+ (djula:compile-template* "base.html"))
(defparameter +welcome.html+ (djula:compile-template* "welcome.html"))
~~~

A Djula template looks like this:

```html
{% extends "base.html" %}
{% block title %}Memberlist{% endblock %}
{% block content %}
  <ul>
  {% for user in users %}
    <li><a href="{{ user.url }}">{{ user.username }}</a></li>
  {% endfor %}
  </ul>
{% endblock %}
```

At last, to render the template, call `djula:render-template*` inside a route.

~~~lisp
(easy-routes:defroute root ("/" :method :get) ()
  (djula:render-template* +welcome.html+ nil
                          :users (get-users)
~~~

Note that for efficiency Djula compiles the templates before rendering them.

It is, along with its companion
[access](https://github.com/AccelerationNet/access/) library, one of
the most downloaded libraries of Quicklisp.

#### Djula filters

Filters allow to modify how a variable is displayed. Djula comes with
a good set of built-in filters and they are [well documented](https://mmontone.github.io/djula/doc/build/html/filters.html). They are not to be confused with [tags](https://mmontone.github.io/djula/doc/build/html/tags.html).

They look like this: `{{ name | lower }}`, where `lower` is an
existing filter, which renders the text into lowercase.

Filters sometimes take arguments. For example: `{{ value | add:2 }}` calls
the `add` filter with arguments `value` and 2.

Moreover, it is very easy to define custom filters. All we have to do
is to use the `def-filter` macro, which takes the variable as first
argument, and which can take more optional arguments.

Its general form is:

~~~lisp
(def-filter :myfilter-name (value arg) ;; arg is optional
   (body))
~~~

and it is used like this: `{{ value | myfilter-name }}`.

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

The author finds it is easier to compose the HTML in separate
functions and macros than with the more famous cl-who. But it
has more features under it sleeves:

- it warns on invalid tags and attributes
- it can automatically number headers, given their depth
- it pretty prints html per default, with control over line breaks
- it understands embedded markdown
- it can tell where in the document a generator function is (see `get-html-tag`)
