+++
title = "the first build"
weight = 300
+++

We have developped a web app in Common Lisp.

At the first step, we opened a REPL and since then, without noticing,
we have compiled variables and function definitions pieces by pieces,
step by step, with keyboard shortcuts giving immediate feedback,
testing our progress on the go, running a function in the REPL or
refreshing the browser window.

{{% notice info %}}

We didn't have to restart any Lisp process, nor any web server.

Think about it, that's awesome!

{{% /notice %}}

*(and yet, we didn't talk about the interactive debugger and about unwinding the stack to resume from errors)*

However, it *is* useful to start our application from scratch once in a while:

- did we list all our dependencies in the .asd project definition?
- does it work from scratch, do we have any issue with fresh data?

We can run our app from sources, and we can build a self-contained binary.

## Run from sources

Do you remember what we did at the beginning to load our project?

- compile and load the .asd project definition
- quickload our project
- start the web server.

That's all we need. Let's write these 3 commands in a new file. At the
project root, create a file `run.lisp`, in which you copy those steps:

```lisp
;; run.lisp
(load "myproject.asd")

(ql:quickload "myproject")

(myproject::start-server)
```

Why the double `::` in `myproject::start-server`? Because we didn't
`export` the `start-server` function, and because we didn't write
`(in-package :myproject)`.

This works too:

```lisp
(in-package :myproject)
(start-server)
```

Now run the app from the terminal:

    sbcl --load run.lisp

or use `rlwrap sbcl` (a "readline wrapper") for convenience, to get REPL history and to support the arrow keys. Yeah the default SBCL REPL is barebones.

## "Address in use" error and interactive debugger

You should see this output, but you shouldn't be worried since the error message is explicit:

```
$ rlwrap sbcl --load run.lisp
This is SBCL 2.1.5, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.
To load "myproject":
  Load 1 ASDF system:
    myproject
; Loading "myproject"
..............
Starting the web server on port 8899
While evaluating the form starting at line 5, column 0
  of #P"/home/vince/projets/web-apps-in-lisp/walk/walk-book/content/tutorial/run.lisp":

debugger invoked on a USOCKET:ADDRESS-IN-USE-ERROR in thread
#<THREAD "main thread" RUNNING {10015484D3}>:
  Condition USOCKET:ADDRESS-IN-USE-ERROR was signalled.

Type HELP for debugger help, or (SB-EXT:EXIT) to exit from SBCL.

restarts (invokable by number or by possibly-abbreviated name):
  0: [RETRY   ] Retry EVAL of current toplevel form.
  1: [CONTINUE] Ignore error and continue loading file "/home/vince/projets/web-apps-in-lisp/walk/walk-book/content/tutorial/run.lisp".
  2: [ABORT   ] Abort loading file "/home/vince/projets/web-apps-in-lisp/walk/walk-book/content/tutorial/run.lisp".
  3:            Ignore runtime option --load "run.lisp".
  4:            Skip rest of --eval and --load options.
  5:            Skip to toplevel READ/EVAL/PRINT loop.
  6: [EXIT    ] Exit SBCL (calling #'EXIT, killing the process).

(USOCKET:SOCKET-LISTEN #(0 0 0 0) 8899 :REUSEADDRESS T :REUSE-ADDRESS NIL :BACKLOG 50 :ELEMENT-TYPE (UNSIGNED-BYTE 8))
   source: (ERROR C)
0]
```

What you got here is the interactive debugger with "restart" actions,
however presented in a kludgy form. What we get in a good Lisp IDE
is way easier to use ;) To exit, read the available restarts, you
have two options: type "2" or "6" in the prompt and press Enter. You
now get another prompt:

```
 6

*
```

This is the top-level Lisp REPL. You can type any Lisp forms in
there. You can press C-d or type `(quit)`.

If you wanted to avoid the debugger, you could use an SBCL switch:
`--non-interactive`. But wait a bit before we use it together please.

Back to our error message.

The meaningful bit is "Condition USOCKET:ADDRESS-IN-USE-ERROR was
signalled". `usocket` is the low-level library that does the
networking for Hunchentoot. "Address in use" is correct: our app is
already running from our editor, already using the default port
number. We have to either stop the app there, either use another port.

To stop the app, use `(hunchentoot:stop *server*)` in our editor REPL
and from within the project package, since `*server*` is a variable of
ours.

To use another port, what would you prefer?

- we can give the port as an argument on the command line, like `--port 9000`.
- we can find the next available port.

Let's do the latter.


## Find a port number

This will be quick as we'll use a library for that, called `find-port`.

Please quickload it and add it to the project definition.

You can either:

- run `(ql:quickload "find-port")` on the REPL and add "find-port" in the .asd, in the ":depends-on" list,
- add "find-port" in the .asd, re-compile it with `C-c C-k` or a call to `load`, and then `(ql:quickload "myproject")`.

{{% notice info %}}

You just loaded a new library without needing to restart the lisp process.

{{% /notice %}}

Our .asd file now looks like:

```lisp
(asdf:defsystem "myproject"
  :version "0.1"
  :author "me"
  :license "WTFPL"
  :depends-on (
               :hunchentoot  ;; web server
               :easy-routes  ;; routes facility
               :djula        ;; HTML templates

               ;; utils
               :find-port    ;;                 <------- added
               )
  :components ((:module "src"  ;; a src/ subdirectory
                :components
                (
                 (:file "myproject") ;; = src/myproject.lisp
                )))

  :description "A list of products")
```

How does `find-port` work? On the REPL, write `(find-port:find-port`
and look at your editor's minibuffer, or tooltip, as it should show
you the function signature. `find-port` takes key arguments,
`:min` and `:max`.

Let's try:

```lisp
MYPROJECT> (find-port:find-port :min 8899)
8900
```

That's all we need to use.

Where should we call it, in the run.lisp file or in myproject.lisp?

We'll create a new function in myproject.lisp. We don't alter the
`start-server` function which does a good job, we'll create a new
function that is responsible of dealing with the outside world.

Let's call this function `main`:

```lisp
;; myproject.lisp
;;; Top-level.
(defun main ()
  (start-server :port (find-port:find-port :min *port*)))
```

And now we use it in the run.lisp file:

```lisp
(load "myproject.asd")

(ql:quickload "myproject")

(in-package :myproject)

(main)
```

Run it again:

    sbcl --load run.lisp

```
This is SBCL 2.1.5, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.
To load "myproject":
  Load 1 ASDF system:
    myproject
; Loading "myproject"
..................................................
[package myproject].
Starting the web server on port 8900
*
```

Now the meaningful message is "Starting the web server on port 8900".

You notice this last `* ` thing? Once again, it's the top-level Lisp
prompt. You can type any Lisp form. You can interact with the running
web app.

Visit [http://localhost:8900/?query=one](http://localhost:8900/?query=one), it works!

You can see HTTP logs in the console:

```
127.0.0.1 - [2025-01-03 13:38:17] "GET /?query=one HTTP/1.1" 200 503 "-" "Mozilla/5.0 (X11; Linux x86_64; rv:124.0) Gecko/20100101 Firefox/124.0"
127.0.0.1 - [2025-01-03 13:38:18] "GET /favicon.ico HTTP/1.1" 404 301 "http://localhost:8900/?query=one" "Mozilla/5.0 (X11; Linux x86_64; rv:124.0) Gecko/20100101 Firefox/124.0"
```

## Interact with the running web app

`* ` is the Lisp prompt. What is the current package?

Type in

    *package*

it responds "#<PACKAGE "COMMON-LISP-USER">".

Are you surprised it isn't our "myproject" package? I am with you. The
explanation is that the `in-package` we wrote in the run.lisp file is
*for this file*. When SBCL starts up, it starts its top-level
evaluator/compiler in the cl-user package. It is asked to load our
file, which it does, but once it's done it's back at the cl-user
package.

To facilitate our interactions with the running app, we can of course
type right now `(in-package :myproject)`, and we can also add an
argument to the command line:

    sbcl --load run.lisp --eval "(in-package :myproject)"

Ask the value of `*package*` again. OK.

What can we do more? We can inspect some variables and test some
functions. Type in `(products)`, it works. You can check the value of
`*server*`. This is actually useful: we are inspecting the internals
of our running web app, and we didn't need to do anything
special. Sure, we can add some logging and a monitoring dashboard. But
as a second step.

What can we do more? We can also re-define our functions and
variables, or develop our app. Look, we can create a new route. Copy
this to the REPL:

```
* (easy-routes:defroute hello-route ("/hello") () "hello new route")
HELLO-ROUTE
```

and go to [http://localhost:8900/hello](http://localhost:8900/hello):
you just created a new route while the app was running.

But wait, do we *really* want to develop our app from this limited
terminal REPL? No! If you don't already, it's time you understand the
usefulness of the `load` function.

What we want is to edit our .lisp source file, instead of copy-pasting
stuff in the REPL, and then to reload the app. The reloading is done
with

    * (load "src/myproject.lisp")

Try it!

You can also use `ql:quickload`.

In doing so, you are re-discovering a less interactive way of
developping.

## Building our first binary

Running from sources is OK.

Building a binary requires a bit more work but it can bring advantages:

- the app will start faster, way faster with a binary (binaries start-up in ±2ms)
- it may be easier to deploy your app to a server
  - because you don't need to install a lisp implementation nor to set up Quicklisp on the server.
- it's easier to ship your app to end users
- you can save working versions of your app: binary-v0.1, binary-v0.2 etc.

Building binaries with SBCL is done with the function
`sb-ext:save-lisp-and-die` (it lives in the `sb-ext` SBCL module, that
is available by default).

Other implementations don't define the exact same function, for
instance on Clozure CL the function is `ccl:save-application`. That's
why we'll want a compatibility layer to write a portable script across
implementations. It is as always provided by ASDF with
`uiop:dump-image` and also with a system declaration in the .asd
files.

SBCL binaries are portable from and to the same operating system:
build on GNU/Linux, run on GNU/Linux. Or build on a CI system on the 3
platforms. They are not truly static binaries as they rely on the
GLibc. There was an ongoing patch to make them truly static, it isn't
done though.

An SBCL binary will weight, by default, around 80MB. With
compression, they get to ±20MB. As your application grows, they'll
stay roughly this size. An app of mine, with dozens of dependencies
and all the application code, templates and static assets (JS and CSS)
is 35MB. LispWorks binaries, reduced in size with their tree shaker,
are known to be smaller, a hello world being ±5MB, a web app around
10MB. This tree shaker isn't in the free version.

Enough talk, let's do it. Create a new `build.lisp` file. We need these steps:

- load our app
- build a binary
  - where we define an entry point

### save-lisp-and-die

`sb-ext:save-lisp-and-die` expects these arguments:

- a binary name, as a string.
- a `:toplevel` key argument, designing the function to run when the binary starts.
- `:executable` to set to `t` if we build an executable, and not a *core image*.
- `:compression` for a compression level (optionnal),
- and more.

We use it like this:

```lisp
(sb-ext:save-lisp-and-die "myproject" :executable t :toplevel #'myproject::main)
```

This is our build.lisp file:

```lisp
(load "myproject.asd")

(ql:quickload "myproject")

(sb-ext:save-lisp-and-die "myproject" :executable t :toplevel #'myproject::main)
```

Now run it with

    sbcl --load build.lisp

You should see:

```
$ sbcl --load build.lisp
This is SBCL 2.1.5, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.
To load "myproject":
  Load 1 ASDF system:
    myproject
; Loading "myproject"
............
[undoing binding stack and other enclosing state... done]
[performing final GC... done]
[defragmenting immobile space... (fin,inst,fdefn,code,sym)=2118+1401+24925+24417+26979... done]
[saving current Lisp image into myproject:
writing 0 bytes from the read-only space at 0x50000000
writing 1696 bytes from the static space at 0x50100000
writing 61177856 bytes from the dynamic space at 0x1000000000
writing 2383872 bytes from the immobile space at 0x50200000
writing 16257024 bytes from the immobile space at 0x52a00000
done]
```

it's `done`. Run `ls` on the project root:

```
$ ls -lh
total 80316
drwxrwxr-x 2 vince vince 4.0K Jan  3 14:35  src
-rw-rw-r-- 1 vince vince  133 Jan  3 14:35  build.lisp
-rwxr-xr-x 1 vince vince  79M Jan  3 14:29  myproject
-rw-rw-r-- 1 vince vince  748 Jan  3 13:36  myproject.asd
-rw-rw-r-- 1 vince vince   84 Jan  3 14:35  run.lisp
```

Make the binary executable with `chmod + myproject` and run it:

```
$ ./myproject
Starting the web server on port 8900
$
```

uuuuh does that look like working?

### "The server exits right after start up!"

The binary was started, it started our web app correctly, but if we
are not mistaken it closed it right away O_o What's going on?

This is a difference from running the app from sources or from a binary:

- from sources, and by default, when you run `sbcl --load myapp.lisp` the myapp.lisp file is loaded and executed, and then we are offered a Lisp REPL (unless we specified `--non-interactive`).
- from a binary, we are *not* offered a Lisp REPL.
  - and that's good, we are in "production mode" now.

We then have to make our web server wait forever.

Just do this, in the `main` function:

```lisp
(defun main ()
  (start-server :port (find-port:find-port :min *port*))
  (sleep most-positive-fixnum))
```

We are sleeping, I forgot, a few million years. The top-level process
is sleeping, but our web server is active in its own thread in the
background.

Build the binary again, run it again… it works :)

```
$ ./myproject
Starting the web server on port 8900
127.0.0.1 - [2025-01-03 14:47:40] "GET /product/1 HTTP/1.1" 200 54 "http://localhost:8900/?query=on" "Mozilla/5.0 (X11; Linux x86_64; rv:124.0) Gecko/20100101 Firefox/124.0"
```

If you have a web server, try sending the binary there and run it. The
app will be available to the internet, you can access it with your
VPS's IP + the app port. It is possible to set the address of the
Hunchentoot acceptor, and to restrict it to localhost, if you wish.

Once your app is deployed, there are a couple ways to interact with it
while it is running, even from the comfort of your editor, at home
(with a Swank server).

Let's try the binary compression.

## Core compression

Compression is done with zlib. Compression levels are comprised
between -1 and 9. There are small differences in the results.

Let's try:

```lisp
;; build.lisp
(sb-ext:save-lisp-and-die "myproject"
                          :executable t
                          :toplevel #'myproject::main
                          :compression 9)
```

The build takes a few more seconds. You see:

```
[saving current Lisp image into myproject:
writing 0 bytes from the read-only space at 0x50000000
compressed 0 bytes into 8 at level 9
writing 1344 bytes from the static space at 0x50100000
compressed 32768 bytes into 422 at level 9
writing 66191360 bytes from the dynamic space at 0x1000000000
…
done
```

`myproject` now weights 19MB.

Because we used templates as strings in our lisp files, our binary is
self-contained by default, without extra work: it contains the lisp
implementation with its compiler and debugger, the libraries (web
server and all), the templates. We can easily deploy this
app. Congrats!


## Closing words

We are only scratching the surface of what we'll want to do with a real app:

- parse CLI args
- handle a `C-c` and other signals
- read configuration files
- setup and use a database
- properly use HTML templates
- add CSS and other static assets
- add interactivity on the web page, with or without JavaScript
- add users login
- add rights to the routes
- build a REST API
- etc

We will explore those topics in the other chapters of this guide.

We did a great job for a first app:

- we built a Common Lisp web app
- we created routes, used path and URL parameters
- we defined an HTML form
- we used HTML templates
- we experienced the interactive nature of Common Lisp
- we explored how to run, build and ship Common Lisp programs.

That's a lot. You are ready for serious applications now!
