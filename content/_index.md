
## Web Apps in Lisp: Know-how

You want to write a web application in Common Lisp and you don't know
where to start? Or you don't want to re-invent the wheel? Follow the guide.


## Why Common Lisp

For web development as for any other task, one can leverage Common
Lisp's advantages:

- the **unmatched REPL** that even helps to interact with a running web app on your remote server,
- the **exception handling system**,
- **performance**,
- easy deployment: ability to build **self-contained executables** containing all our static files,
- **stability**,
- **good threads story**,
- **strong** and incremental **typing**,
- etc.

All the development experience is very much interactive, allowing us
to write and test features right away. **There are no compile times**
during development, we only need to compile the app from scratch once in
a while, to check that our dependencies or the project layout are
correctly declared.

We can, for example, define a new route and try it right away, **we
don't wait for a local web server** to pick up the changes and
restart. In case of an error, the interactive debugger pops up (if we
want to), **no server will crash because of an undefined variable**
(looking at you, Python). We can edit a function and compile it with a
keyboard shortcut (the usual `C-c C-c` in Slime): we can compile our
code *one function at a time*. The **feedback is
immediate**.

We can **choose the degree of interactivity**: the web server
can catch exceptions and fire the interactive debugger, or print lisp
backtraces on the browser, or display a 404 error page and print logs
on standard output. The ability to build self-contained executables
eases deployment tremendously (compared to, for example, npm-based
apps), in that we just copy the executable to a server and run it.

But we can also run it as a "script" from sources.

In both cases, when we have deployed it, we can still **interact
with the running application**, allowing for **hot code reload**. We
can even install new dependencies while it is running, no process has
to restart.  Of course, if we prefer to be careful and we don't want to use live
reload capabilities, we might still enjoy this capability to reload, for example,
a user's configuration file.

So Common Lisp can be a good choice for web development too. The
ecosystem has lots of little or not so little libraries, including a
production-ready, battle proven web framework (Hunchentoot).

However, **we don't have a batteries-included web framework easily
usable by novice web developers** (yet…). But now you have this guide.

## What's in this guide

We'll present here some established web frameworks and other common
libraries to help you getting started in developing a web
application. We do *not* aim to be exhaustive nor to replace the
upstream documentation.

We will learn:

- how to start a simple web project with the Hunchentoot web server
  - defining routes
  - returning any content type, including JSON
  - serving HTML
    - HTML will be written in HTML, with a Django-like template engine (Djula), or a more flexible template engine (Ten, but Djula is already very flexible)
    - we will show how to write HTML in s-expressions (with Spinneret)
- how to connect to a database, how to define models
  - how to serialize and deserialize the models to and from JSON
- how to debug a running web app
- how to serve JavaScript
  - how to make AJAX calls to our Lisp backend
    - with pure JavaScript
    - with Vue.js
    - with the ISSR lisp library
    - with the HTMX library
- how to create user login
- how to build and deploy a project
  - with executables
  - with SystemD
  - how to configure Nginx
  - how to setup third-party service providers (Sentry, Plausible web analytics, Graphana…)
- how to set up a Continuous Integration system
- how to build a Debian package.

Your feedback and contributions are appreciated!

**In [part 1](/part-1/), we will get to know a classical stack: Hunchentoot, easy-routes for easier routing and Djula templates.**

In part 2, we will add interactivity on the client side, with or without JavaScript.

In part 3, we will build an interactive Ajax-based Todo-app without writing any JavaScript, thanks to ISSR and Weblocks.

Now let's start with a [libraries overview](/part 1/).


{{% notice note  %}}
🎥 If you need to learn Common Lisp, good news, [I am creating this video course on Udemy](https://www.udemy.com/course/common-lisp-programming/?referralCode=2F3D698BBC4326F94358).
{{% /notice %}}

{{% notice info  %}}

If you find similar content from the Cookbook, this is normal. I am the main contributor and I wrote the web page there. This guide wants to be more complete and more advanced.

{{% /notice %}}


{{% notice info %}}

Here's a 5 minutes refresh on how to create a project, how to run it from sources, how to build a binary and how to load everything in SLIME:

{{% /notice %}}

{{< youtube XFc513MJjos >}}


[hunchentoot]: https://edicl.github.io/hunchentoot
[clack]: https://github.com/fukamachi/clack
[caveman]: https://github.com/fukamachi/caveman
[radiance]: https://github.com/Shirakumo/radiance
[snooze]: https://github.com/joaotavora/snooze
[cl-rest-server]: https://github.com/mmontone/cl-rest-server
[weblocks]: https://github.com/40ants/weblocks
