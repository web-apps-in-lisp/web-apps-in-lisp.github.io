+++
title = "Weblocks: solving the JavaScript Problem©"
weight = 100
+++

[Weblocks][weblocks] is a widgets-based and server-based framework
with a built-in ajax update mechanism. It allows to write dynamic web
applications **without the need to write JavaScript or to write lisp
code that would transpile to JavaScript**. It is thus super
exciting. It isn't for newcomers however.

It isn't also the only solution that aims at makin writing interactive web apps easier, where the client logic can be brought to the back-end. See also:

- [CLOG](https://github.com/rabbibotton/clog)
- [ISSR](https://github.com/interactive-ssr/issr-server)
- and others that we will eventually add.

The Weblocks's demo we will build is a TODO app:

![](http://40ants.com/weblocks/_images/quickstart-check-task.gif)

Weblocks is an old framework developed by Slava Akhmechet, Stephen
Compall and Leslie Polzer. After nine calm years, it is seeing a very
active update, refactoring and rewrite effort by Alexander Artemenko.

It was initially based on continuations (they were removed to date)
and thus a lispy cousin of Smalltalk's
[Seaside](https://en.wikipedia.org/wiki/Seaside_(software)). We can
also relate it to Haskell's Haste, OCaml's Eliom,
Elixir's Phoenix LiveView and others.

The [Ultralisp](http://ultralisp.org/) website is an example Weblocks
website in production known in the CL community.

---

{{% notice note %}}

To install Weblocks, please see its documentation. The Weblocks in Quicklisp is not yet, as of writing, the one we are interested in.

{{% /notice %}}


Weblock's unit of work is the *widget*. They look like a class definition:

~~~lisp
(defwidget task ()
   ((title
     :initarg :title
     :accessor title)
    (done
     :initarg :done
     :initform nil
     :accessor done)))
~~~

Then all we have to do is to define the `render` method for this widget:

~~~lisp
(defmethod render ((task task))
  "Render a task."
  (with-html
        (:span (if (done task)
                   (with-html
                         (:s (title task)))
                 (title task)))))
~~~

It uses the Spinneret template engine by default, but we can bind any
other one of our choice.

> Note: I'd like to use Djula HTML templates with Weblocks. Example welcome.

To trigger an ajax event, we write lambdas in full Common Lisp:

~~~lisp
...
(with-html
  (:p (:input :type "checkbox"
    :checked (done task)
    :onclick (make-js-action
              (lambda (&key &allow-other-keys)
                (toggle task))))
...
~~~

The function `make-js-action` creates a simple javascript function
that calls the lisp one on the server, and automatically refreshes the
HTML of the widgets that need it. In our example, it re-renders one
task only.

Is it appealing ? Carry on this quickstart guide here: [http://40ants.com/weblocks/quickstart.html](http://40ants.com/weblocks/quickstart.html).


[weblocks]: https://github.com/40ants/reblocks
