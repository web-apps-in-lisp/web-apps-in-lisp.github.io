+++
title = "Remote debugging"
weight = 205
+++

You can have your software running on a machine over the network,
connect to it and debug it from home, from your development
environment.

You can not only inspect the running program, but also compile and
load new code, including installing new libraries, effectively doing
hot code reload. It's up to you to decide to do it or to follow the
industry's best practices. It isn't because you use Common Lisp that
you have to make your deployed program a "big ball of mud".

Also don't be too afraid, because when you connect to the remote
application from your development machine and start writing code, you
are writing code on your local files, in your git project. When you
compile and load it with the usual shortcuts (C-c C-c), you are
sending the changes to the remote app.

TLDR; local changes, remote execution.


{{% notice info %}}
We are re-using content we contributed to the Cookbook.
{{% /notice %}}

The steps involved are to start a **Swank server** on the remote machine (Swank is the backend companion of Slime), create an
ssh tunnel and connect to the Swank server from our editor. Then we
can browse and evaluate code on the running instance transparently.

To test this, let's define a function that prints forever.

If needed, import the dependencies first:

~~~lisp
(ql:quickload '("swank" "bordeaux-threads"))
~~~


~~~lisp
;; a little common lisp swank demo
;; while this program is running, you can connect to it from
;; another terminal or machine
;; and change the definition of doprint to print something else out!

(require :swank)
(require :bordeaux-threads)

(defparameter *counter* 0)

(defun dostuff ()
  (format t "hello world ~a!~%" *counter*))

(defun runner ()
  (swank:create-server :port 4006 :dont-close t)
  (format t "we are past go!~%")
  (bt:make-thread (lambda ()
                    (loop repeat 5 do
                          (sleep 5)
                          (dostuff)
                          (incf *counter*)))
                  :name "do-stuff"))

(runner)
~~~

On the server, we can run this code with

    sbcl --load demo.lisp

If you check with `(bt:all-threads)`, you'll see your Swank server running on port 4006, as well
as the other thread ready to do stuff:

    (#<SB-THREAD:THREAD "do-stuff" RUNNING {10027CEDC3}>
     #<SB-THREAD:THREAD "Swank Sentinel" waiting on:
          #<WAITQUEUE  {10027D0003}>
        {10027CE8B3}>
     #<SB-THREAD:THREAD "Swank 4006" RUNNING {10027CEB63}>
     #<SB-THREAD:THREAD "main thread" RUNNING {1007C40393}>)

We do port forwarding on our development machine:

    ssh -L4006:127.0.0.1:4006 username@example.com

this will securely forward port 4006 on the server at example.com to
our local computer's port 4006 (Swank only accepts connections from
localhost).

We connect to the running Swank with `M-x slime-connect`, choosing localhost for the host
and port 4006.

We can write new code:

~~~lisp
(defun dostuff ()
  (format t "goodbye world ~a!~%" *counter*))
(setf *counter* 0)
~~~

and eval it as usual with `C-c C-c` or `M-x slime-eval-region` for instance. The output should change.

That's how Ron Garret debugged the Deep Space 1 spacecraft from the earth
in 1999:

> We were able to debug and fix a race condition that had not shown up during ground testing. (Debugging a program running on a $100M piece of hardware that is 100 million miles away is an interesting experience. Having a read-eval-print loop running on the spacecraft proved invaluable in finding and fixing the problem.

## References

- [Slime documentation: connecting to a remote Lisp](https://common-lisp.net/project/slime/doc/html/Connecting-to-a-remote-lisp.html#Connecting-to-a-remote-lisp)
- [Ron Garret: Lisping at the JPL](http://www.flownet.com/gat/jpl-lisp.html#1994-1999%20-%20Remote%20Agent)
- [CL Cookbook: debugging](https://lispcookbook.github.io/cl-cookbook/debugging.html)
