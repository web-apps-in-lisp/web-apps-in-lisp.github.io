+++
title = "Part 1: the first route"
weight = -1
+++

It's time we create a web app!

## Our first route

Our very first route will only respond with a "hello world".

Let's start with a couple variables.

```lisp
;; still in src/myproject.lisp
(defvar *server* nil
  "Server instance (Hunchentoot acceptor).")

(defparameter *port* 8899 "The application port.")
```

Now hold yourself and let's write our first route:

```lisp
(easy-routes:defroute root ("/") ()
    "hello app")
```

It only returns a string. We'll use HTML templates in a second.

It's time to start our web server:

```lisp
(defun start-server (&key (port *port*))
  (format t "~&Starting the web server on port ~a" port)
  (force-output)
  (setf *server* (make-instance 'easy-routes:easy-routes-acceptor
                                :port port))
  (hunchentoot:start *server*))
```

Compile the whole file with `C-c C-k` or each individual new variable
and function with `C-c C-c`. Alternatively, you can always do `(load
"src/myproject.lisp")` in any REPL. You don't need to quit and restart
the running Lisp image.


## Start the app

Now call the function `(myproject::start-server)` or simply
`(start-server)` if you did the `(in-package :myproject`) in the REPL.

Yourather instantly) should see:

```
CL-USER> (myproject::start-server )
Starting the web server on port 8899
#<EASY-ROUTES:EASY-ROUTES-ACCEPTOR (host *, port 8899)>
```

So open your browser at [localhost:8899/](localhost:8899/).

{{% notice info %}}

In Slime, the shortcut `C-c C-y` aka `M-x slime-call-defun` automatically inserts the function at point in the REPL.

{{% /notice %}}

That's how you get a web app running in Common Lisp. The rest is
regular web work: HTML templates, forms, etc.

So what if we added a bit more of HTML?

By the way, did you notice that while developping, you didn't have to stop the
app, to stop the web server, nor to reload anything? Any changes
compiled in the running image are immediately available.

To stop the app, use `(hunchentoot:stop *server*)`. You can put this in a "stop-app" function.
