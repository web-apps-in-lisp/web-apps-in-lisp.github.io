+++
title = "Reblocks"
weight = 100
+++

[Reblocks][reblocks] is a widgets-based and server-based framework
with a built-in ajax update mechanism. It allows to write dynamic web
applications **without the need to write JavaScript or to write lisp
code that would transpile to JavaScript**. It is thus super
exciting. It isn't for newcomers however.

The Reblocks's demo we will build is a TODO app:

![](https://40ants.com/reblocks/images/docs/images/quickstart-check-task.gif)

---

{{% notice note %}}

To install Reblocks, please see its documentation.

{{% /notice %}}


Reblocks' unit of work is the *widget*. They look like a class definition:

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

Is it appealing ? [Carry on its quickstart guide](https://40ants.com/reblocks/quickstart/#x-28REBLOCKS-2FDOC-2FQUICKSTART-3A-40QUICKSTART-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29).


[reblocks]: https://github.com/40ants/reblocks
