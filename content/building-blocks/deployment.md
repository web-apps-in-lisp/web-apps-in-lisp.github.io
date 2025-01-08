+++
title = "Deployment"
weight = 200
+++


How to deploy and monitor a Common Lisp web app?

{{% notice info %}}
We are re-using content we contributed to the Cookbook.
{{% /notice %}}


## Deploying manually

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


## SystemD: daemonizing, restarting in case of crashes, handling logs

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


## With Docker

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

### Running behind Nginx

There is nothing CL-specific to run your Lisp web app behind Nginx. Here's an example to get you started.

We suppose you are running your Lisp app on a web server, with the IP
address 1.2.3.4, on the port 8001. Nothing special here. We want to
access our app with a real domain name (and eventuall benefit of other
Nginx's advantages, such as rate limiting etc). We bought our domain
name and we created a DNS record of type A that links the domain name
to the server's IP address.

We must configure our server with Nginx to tell it that all
connections coming from "your-domain-name.org", on port 80, are to be
sent to the Lisp app running locally.

Create a new file: `/etc/nginx/sites-enabled/my-lisp-app.conf` and add this proxy directive:

~~~lisp
server {
    listen www.your-domain-name.org:80;
    server_name your-domain-name.org www.your-domain-name.org;  # with and without www
    location / {
        proxy_pass http://1.2.3.4:8001/;
    }

    # Optional: serve static files with nginx, not the Lisp app.
    location /files/ {
        proxy_pass http://1.2.3.4:8001/files/;
    }
}
~~~

Note that on the proxy_pass directive: `proxy_pass
http://1.2.3.4:8001/;` we are using our server's public IP
address. Often, your Lisp webserver such as Hunchentoot directly
listens on it. You might want, for security reasons, to run the Lisp
app on localhost.

Reload nginx (send the "reload" signal):

    $ nginx -s reload

and that's it: you can access your Lisp app from the outside through `http://www.your-domain-name.org`.


## Deploying on Heroku, Digital Ocean, OVH, Deploy.sh and other services

See:

* [heroku-buildpack-common-lisp](https://gitlab.com/duncan-bayne/heroku-buildpack-common-lisp)
* [Platform.sh](https://platform.sh/blog/2019/lisp/) has Common Lisp support and so has [OVH](https://docs.ovh.com/ie/en/web-paas/languages-lisp/) through their Web PaaS partnership.
* [Heliohost](https://www.heliohost.org/) offer a free shared hosting solution for Common Lisp.
* see the [Awesome CL list #deploy section](https://github.com/CodyReichert/awesome-cl#deployment) for more interface libraries for Kubernetes, OpenShift, AWS, etc.

### Cloud Init

You can take inspiration from this [Cloud Init file for SBCL](https://git.sr.ht/%7Emarcuskammer/cloudinit/tree/main/item/sbcl-nginx.yml), an init file for providers supporting the cloudinit format (DigitalOcean etc).


## Monitoring

See [Prometheus.cl](https://github.com/deadtrickster/prometheus.cl)
for a Grafana dashboard for SBCL and Hunchentoot metrics (memory,
threads, requests per second,…).

See [cl-sentry-client](https://github.com/mmontone/cl-sentry-client/) for error reporting.

## References

- [https://github.com/CodyReichert/awesome-cl#deployment](https://github.com/CodyReichert/awesome-cl#deployment)
