+++
title = "Running and Building"
weight = 150
+++


## Running the application from source

{{% notice info %}}
See the tutorial.
{{% /notice %}}

To run our Lisp code from source, as a script, we can use the `--load`
switch from our implementation.

We must ensure:

- to load the project's .asd system declaration (if any)
- to install the required dependencies (this demands we have installed Quicklisp previously)
- and to run our application's entry point.

So, the recipe to run our project from sources can look like this (you can find such a recipe in our project generator):

~~~lisp
;; run.lisp

(load "myproject.asd")

(ql:quickload "myproject")

(in-package :myproject)
(handler-case
    (myproject::start-app :port (ignore-errors (parse-integer (uiop:getenv "PROJECT_PORT"))))
  (error (c)
    (format *error-output* "~&An error occured: ~a~&" c)
    (uiop:quit 1)))
~~~

In addition we have allowed the user to set the application's port
with an environment variable.

We can run the file like so:

    sbcl --load run.lisp

After loading the project, the web server is started in the
background. We are offered the usual Lisp REPL, from which we can
interact with the running application.

We can also connect to the running application from our preferred
editor, from home, and compile the changes in our editor to the
running instance. See the following section:
[connecting to a remote lisp image](https://lispcookbook.github.io/cl-cookbook/debugging.html#remote-debugging) on the Cookbook.


## Building a self-contained executable

{{% notice info %}}
See the tutorial.
{{% /notice %}}

As for all Common Lisp applications, we can bundle our web app in one
single executable, including the assets. It makes deployment very
easy: copy it to your server and run it.

```
$ ./my-web-app
Hunchentoot server is started.
Listening on localhost:9003.
```

See this recipe on [scripting#for-web-apps](scripting.html#for-web-apps).

As for any executable, you need this in your .asd file:

~~~lisp
:build-operation "program-op" ;; leave as is
:build-pathname "<binary-name>"
:entry-point "<my-package:main-function>"
~~~

and you build the binary with `(asdf:make :myproject)`.

However, you might find that as soon as you start your app, its
stops. That happens because the server thread is started in the background, and nothing tells the binary to wait for it. We can simply sleep (for a large-enough amount of time).


~~~lisp
(defun main ()
  (start-app :port 9003) ;; our start-app
  ;; keep the binary busy in the foreground, for binaries and SystemD.
  (sleep most-positive-fixnum))
~~~

If you want to learn more, see… [the Cookbook: scripting, command line arguments, executables](https://lispcookbook.github.io/cl-cookbook/scripting.html).

