+++
title = "Deployment"
weight = 200
+++


How to run, build, test, deploy and monitor a Common Lisp web app.

### Running the application from source

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
    ;; The START function starts the web server.
    (myproject::start :port (ignore-errors (parse-integer (uiop:getenv "PROJECT_PORT"))))
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


### Building a self-contained executable

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
stops. That happens because the server thread is started in the background, and nothing tells the binary to wait for it. Let's do it by putting the server thread in the foreground.

You need the `bordeaux-threads` library, which as the `bt` global nickname.

~~~lisp
(defun main ()
  (start-app :port 9003) ;; our start-app
  ;; let the webserver run.
  ;; note: we hardcoded our webserver name, "hunchentoot" here.
  (handler-case (bt:join-thread (find-if (lambda (th)
                                            (search "hunchentoot" (bt:thread-name th)))
                                         (bt:all-threads)))
    ;; Catch a user's C-c
    (#+sbcl sb-sys:interactive-interrupt
      #+ccl  ccl:interrupt-signal-condition
      #+clisp system::simple-interrupt-condition
      #+ecl ext:interactive-interrupt
      #+allegro excl:interrupt-signal
      () (progn
           (format *error-output* "Aborting.~&")
           (clack:stop *server*)
           (uiop:quit)))
    (error (c) (format t "Woops, an unknown error occured:~&~a~&" c))))
~~~

If you want to learn more, see… [the Cookbook: scripting, command line arguments, executables](https://lispcookbook.github.io/cl-cookbook/scripting.html).


<!-- ### Multi-platform delivery with Electron -->

<!-- [Ceramic](https://ceramic.github.io/) makes all the work for us. -->

<!-- It is as simple as this: -->

<!-- ~~~lisp -->
<!-- ;; Load Ceramic and our app -->
<!-- (ql:quickload '(:ceramic :our-app)) -->

<!-- ;; Ensure Ceramic is set up -->
<!-- (ceramic:setup) -->
<!-- (ceramic:interactive) -->

<!-- ;; Start our app (here based on the Lucerne framework) -->
<!-- (lucerne:start our-app.views:app :port 8000) -->

<!-- ;; Open a browser window to it -->
<!-- (defvar window (ceramic:make-window :url "http://localhost:8000/")) -->

<!-- ;; start Ceramic -->
<!-- (ceramic:show-window window) -->
<!-- ~~~ -->

<!-- and we can ship this on Linux, Mac and Windows. -->

<!-- There is more: -->

<!-- > Ceramic applications are compiled down to native code, ensuring both performance and enabling you to deliver closed-source, commercial applications. -->

<!-- Thus, no need to minify our JS. -->

## Deployment

### Deploying manually

We can start our executable in a shell and send it to the background (`C-z bg`), or run it inside a `tmux` session. These are not the best but hey, it works©.

Here's a `tmux` crashcourse:

- start a tmux session with `tmux`
- inside a session, `C-b` is tmux's modifier key.
  - use `C-b c` to create a new tab, `C-b n` and `C-b p` for "next" and "previous" tab/window.
  - use `C-b d` to detach tmux and come back to your original console. Everything you started in tmux still runs in the background.
  - use `C-g` to cancel a current prompt (as in Emacs).
- `tmux ls` lists the running tmux sessions.
- `tmux attach` goes back to a running session.
  - `tmux attach -t <name>` attaches to the session named "name".
  - inside a session, use `C-b $` to name the current session, so you can see it with `tmux ls`.

Here's [a cheatsheet](https://tmuxcheatsheet.com/) that was handy.

Unfortunately, if your app crashes or if your server is rebooted, your apps will be stopped. We can do better.


### SystemD: daemonizing, restarting in case of crashes, handling logs

This is actually a system-specific task. See how to do that on your system.

Most GNU/Linux distros now come with Systemd, so here's a little example.

Deploying an app with Systemd is as simple as writing a configuration file:

```
$ emacs -nw /etc/systemd/system/my-app.service
[Unit]
Description=stupid simple example

[Service]
WorkingDirectory=/path/to/your/app
ExecStart=/usr/local/bin/sthg sthg
Type=simple
Restart=always
RestartSec=10
```

Then we have a command to start it:

    sudo systemctl start my-app.service

a command to check its status:

    systemctl status my-app.service


and Systemd can handle **logging** (we write to stdout or stderr, it writes logs):

    journalctl -f -u my-app.service


and it handles crashes and **restarts the app**:

    Restart=always

and it can **start the app after a reboot**:

    [Install]
    WantedBy=basic.target

to enable it:

    sudo systemctl enable my-app.service


### With Docker

There are several Docker images for Common
Lisp. For example:

- [clfoundation's Docker images](https://hub.docker.com/r/clfoundation/sbcl) with Quicklisp (not enabled by default)
- [40ants/base-lisp-image](https://github.com/40ants/base-lisp-image)
is based on Ubuntu LTS and includes SBCL, CCL, Quicklisp, Qlot and
Roswell.
- [container-lisp/s2i-lisp](https://github.com/container-lisp/s2i-lisp)
is CentOs based and contains the source for building a Quicklisp based
Common Lisp application as a reproducible docker image using OpenShift's
source-to-image.


### With Guix

[GNU Guix](https://www.gnu.org/software/guix/) is a transactional
package manager, that can be installed on top of an existing OS, and a
whole distro that supports declarative system configuration. It allows
to ship self-contained tarballs, which also contain system
dependencies. For an example, see the [Nyxt browser](https://github.com/atlas-engineer/nyxt/).


### Deploying on Heroku and other services

See [heroku-buildpack-common-lisp](https://gitlab.com/duncan-bayne/heroku-buildpack-common-lisp) and the [Awesome CL#deploy](https://github.com/CodyReichert/awesome-cl#deployment) section for interface libraries for Kubernetes, OpenShift, AWS, etc.


## Monitoring

See [Prometheus.cl](https://github.com/deadtrickster/prometheus.cl)
for a Grafana dashboard for SBCL and Hunchentoot metrics (memory,
threads, requests per second,…).

## Connecting to a remote Lisp image

This this section: [debugging#remote-debugging](https://lispcookbook.github.io/cl-cookbook/debugging.html#remote-debugging) on the Cookbook.

## Hot reload

This is an example from [Quickutil](https://github.com/stylewarning/quickutil/blob/master/quickutil-server/). It is actually an automated version of the precedent section.

It has a Makefile target:

```lisp
hot_deploy:
	$(call $(LISP), \
		(ql:quickload :quickutil-server) (ql:quickload :swank-client), \
		(swank-client:with-slime-connection (conn "localhost" $(SWANK_PORT)) \
			(swank-client:slime-eval (quote (handler-bind ((error (function continue))) \
				(ql:quickload :quickutil-utilities) (ql:quickload :quickutil-server) \
				(funcall (symbol-function (intern "STOP" :quickutil-server))) \
				(funcall (symbol-function (intern "START" :quickutil-server)) $(start_args)))) conn)) \
		$($(LISP)-quit))
```

It has to be run on the server (a simple fabfile command can call this
through ssh). Beforehand, a `fab update` has run `git pull` on the
server, so new code is present but not running. It connects to the
local swank server, loads the new code, stops and starts the app in a
row.
