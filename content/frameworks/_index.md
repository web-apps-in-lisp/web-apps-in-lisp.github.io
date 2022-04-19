+++
title = "Frameworks overview"
weight = 10
+++


[Hunchentoot][hunchentoot] and [Clack][clack] are two projects that
you'll often hear about.

{{% notice info %}}

CL-USER> `(ql:quickload "hunchentoot")`

{{% /notice %}}

Hunchentoot is

> a web server and at the same time a toolkit for building dynamic websites. As a stand-alone web server, Hunchentoot is capable of HTTP/1.1 chunking (both directions), persistent connections (keep-alive), and SSL. It provides facilities like automatic session handling (with and without cookies), logging, customizable error handling, and easy access to GET and POST parameters sent by the client.

It is a software written by Edi Weitz (author of the "Common Lisp Recipes" book,
of the ubiquitous `cl-ppcre` library and [much more](https://edicl.github.io/)), it is used and
proven solid. One can achieve a lot with it, but sometimes with more
friction than with a traditional web framework. For example,
dispatching a route by the HTTP method is a bit convoluted, one must
write a function for the `:uri` parameter that does the check, when it
is a built-in keyword in other frameworks. We will use
the `easy-routes` library for that.

Clack is

> a web application environment for Common Lisp inspired by Python's WSGI and Ruby's Rack.

Also written by a prolific lisper
([E. Fukamachi](https://github.com/fukamachi/)), it actually uses
Hunchentoot by default as the server, but thanks to its pluggable
architecture one can use another web server, like the asynchronous
[Woo](https://github.com/fukamachi/woo), built on the
[libev](http://software.schmorp.de/pkg/libev.html) event loop, maybe
"the fastest web server written in any programming language"©.

We'll cite also [Wookie](https://github.com/orthecreedence/wookie), an asynchronous HTTP server, and its
companion library
[cl-async](https://github.com/orthecreedence/cl-async), for general
purpose, non-blocking programming in Common Lisp, built on libuv, the
backend library in Node.js.

Clack being more recent and less documented, and Hunchentoot a
"de-facto standard", we'll concentrate on the latter for this
guide. Your contributions to add support for Clack are of course welcome.

Web frameworks build upon web servers and can provide facilities for
common activities in web development, like a templating system, access
to a database, session management, or facilities to build a REST
api (those are all easy to do from a bare bones Hunchentoot, as you
will see). Caveman is the most popular as per the GitHub stars, but not
the most capable. Snooze is maybe the most downloaded Common Lisp web
server. In this guide we will learn how to use Hunchentoot, with some
light libraries on top.

In the end, we will introduce [Weblocks][weblocks]. It is a
  venerable Common Lisp web framework that permits to write ajax-based
  dynamic web applications without writing any JavaScript, nor writing
  some lisp that would transpile to JavaScript, thanks to its
  components-based architecture.  It is seeing an extensive rewrite
  and update since 2017.  We present it in more details in the
  "Isomorphic web frameworks" appendix.

For a full list of libraries for the web, please see the [awesome-cl list
#network-and-internet](https://github.com/CodyReichert/awesome-cl#network-and-internet)
and [Cliki](https://www.cliki.net/Web).

**In [part 1](/part-1/), we will use a classical stack: Hunchentoot, easy-routes for easier routing and Djula templates.**

In part 2, we will add Ajax interactivity on the client side with some pure JavaScript and some Vue.js.

In part 3, we will build an interactive Ajax-based Todo-app without writing any JavaScript, thanks to ISSR, and Weblocks.


[hunchentoot]: https://edicl.github.io/hunchentoot
[clack]: https://github.com/fukamachi/clack
[caveman]: https://github.com/fukamachi/caveman
[radiance]: https://github.com/Shirakumo/radiance
[snooze]: https://github.com/joaotavora/snooze
[cl-rest-server]: https://github.com/mmontone/cl-rest-server
[weblocks]: https://github.com/40ants/weblocks
