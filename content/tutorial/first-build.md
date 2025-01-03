+++
title = "Part 1: the first build"
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

However, it *is* useful to start our application from scratch once in a while:

- did we list all our dependencies in the .asd project definition?
- does it work from scratch, do we have any issue with new data?

We can run our app from sources, and we can build a self-contained binary.

## Run from sources
