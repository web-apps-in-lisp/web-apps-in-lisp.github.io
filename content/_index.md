

You want to write a web application in Common Lisp and you don't know
where to start? You are a beginner in web development, or a lisp
amateur looking for clear and short pointers about web dev in Lisp?
You are all at the right place.


## What's in this guide

We'll start with a **tutorial** that shows the essential building blocks:

- starting a web server
- defining routes
- grabbing URL parameters
- rendering templates
- and running our app from sources, or building a binary.

We'll build a simple page that presents a search form, filters a list of
products and displays the results.

![](/tutorial/web-app.png?lightbox=false&shadow=true)


The **building blocks** section is organized by topics, so that with a
question in mind you should be able to look at the table of contents,
pick the right page and go ahead. You'll find more tutorials, for
example in "User log-in" we build a login form.

![](/building-blocks/login.png)


We hope that it will be plenty useful to you.

Don't hesitate to share what you're building!

{{% notice info %}} 🎥 If you want to learn Common Lisp efficiently,
with a code-driven approach, good news, [I am creating a video
course](https://www.udemy.com/course/common-lisp-programming/?referralCode=2F3D698BBC4326F94358)
on the Udemy platform. Already more than 7 hours of content, rated
4.61/5 by our learners (thank you!). Learn more and get coupons
[here](https://github.com/vindarel/common-lisp-course-in-videos/). I
also have [Common Lisp tutorial videos on
Youtube](https://www.youtube.com/@vindarel).  {{% /notice %}}

Now let's go to [the tutorial](/tutorial/).

## How to start with Common Lisp

This resource is not about learning Common Lisp the language. We
expect you have a working setup, with the Quicklisp package
manager. Please refer to the [CL
Cookbook](https://lispcookbook.github.io/cl-cookbook/).


## Contact

We are @vindarel on [Lisp's Discord server](https://discord.gg/hhk46CE) and Mastodon.
<!-- Our email: `(@ vindarel (dot mailz org))`. -->

<!-- I am an independent developer, using Common Lisp in production© since 2020 and I am sharing my experince on [https://lisp-journey.gitlab.io/](https://lisp-journey.gitlab.io/). -->


<!-- ## Keywords -->

<!-- We will learn: -->

<!-- - how to set up a project, with an .asd system definition and a package -->
<!-- - how to develop interactively, re-compiling our definitions as we go -->
<!-- - how to start a web project, using the Hunchentoot web server -->
<!--   - defining routes and using query parameters -->
<!--   - serving HTML and rendering templates -->
<!--     - we will mainly write plain HTML with the Djula templating engine -->
<!--     - but we'll also show how to write HTML in s-expressions with the Spinneret library -->
<!-- - how to debug a running web app -->
<!-- - how to connect to a database -->
<!-- - how to create a JSON web API -->
<!-- - how to serve JavaScript and static assets -->
<!--   - how to make AJAX calls to our Lisp backend -->
<!--     - with JavaScript -->
<!--     - with the HTMX library -->
<!--     - with Vue.js -->
<!--     - with an isomorphic Common Lisp web framework -->
<!-- - how to create users with a login page -->
<!-- - how to build and deploy a project -->
<!--   - with executables -->
<!--   - with SystemD -->
<!--   - how to configure Nginx -->


<!-- {{< youtube XFc513MJjos >}} -->


<!-- ## Why Common Lisp -->

<!-- For web development as for any other task, one can leverage Common -->
<!-- Lisp's advantages: -->

<!-- - the **unmatched REPL** that even helps to interact with a running web app on your remote server, -->
<!-- - the **exception handling system**, -->
<!-- - **performance**, -->
<!-- - easy deployment: ability to build **self-contained executables** containing all our static files, -->
<!-- - **stability**, -->
<!-- - **good threads story**, -->
<!-- - **strong** and incremental **typing**, -->
<!-- - etc. -->

<!-- All the development experience is very much interactive, allowing us -->
<!-- to write and test features right away. **There are no compile times** -->
<!-- during development, we only need to compile the app from scratch once in -->
<!-- a while, to check that our dependencies or the project layout are -->
<!-- correctly declared. -->

<!-- We can, for example, define a new route and try it right away, **we -->
<!-- don't wait for a local web server** to pick up the changes and -->
<!-- restart. In case of an error, the interactive debugger pops up (if we -->
<!-- want to), **no server will crash because of an undefined variable** -->
<!-- (looking at you, Python). We can edit a function and compile it with a -->
<!-- keyboard shortcut (the usual `C-c C-c` in Slime): we can compile our -->
<!-- code *one function at a time*. The **feedback is -->
<!-- immediate**. -->

<!-- We can **choose the degree of interactivity**: the web server -->
<!-- can catch exceptions and fire the interactive debugger, or print lisp -->
<!-- backtraces on the browser, or display a 404 error page and print logs -->
<!-- on standard output. The ability to build self-contained executables -->
<!-- eases deployment tremendously (compared to, for example, npm-based -->
<!-- apps), in that we just copy the executable to a server and run it. -->

<!-- But we can also run it as a "script" from sources. -->

<!-- In both cases, when we have deployed it, we can still **interact -->
<!-- with the running application**, allowing for **hot code reload**. We -->
<!-- can even install new dependencies while it is running, no process has -->
<!-- to restart.  Of course, if we prefer to be careful and we don't want to use live -->
<!-- reload capabilities, we might still enjoy this capability to reload, for example, -->
<!-- a user's configuration file. -->

<!-- So Common Lisp can be a good choice for web development too. The -->
<!-- ecosystem has lots of little or not so little libraries, including a -->
<!-- production-ready, battle proven web framework (Hunchentoot). -->

<!-- However, **we don't have a batteries-included web framework easily -->
<!-- usable by novice web developers** (yet…). But now you have this guide. -->

[hunchentoot]: https://edicl.github.io/hunchentoot
[clack]: https://github.com/fukamachi/clack
[caveman]: https://github.com/fukamachi/caveman
[radiance]: https://github.com/Shirakumo/radiance
[snooze]: https://github.com/joaotavora/snooze
[cl-rest-server]: https://github.com/mmontone/cl-rest-server
[weblocks]: https://github.com/40ants/reblocks
