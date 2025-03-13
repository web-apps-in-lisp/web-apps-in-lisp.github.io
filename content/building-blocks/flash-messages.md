+++
title = "Flash messages"
weight = 36
+++

Flash messages are temporary messages you want to show to your
users. They should be displayed once, and only once: on a subsequent
page load, they don't appear anymore.

![](/building-blocks/flash-messages.png)

They should specially work across route redirects. So, they are
typically created in the web session.

Handling them involves those steps:

- create a message in the session
  - have a quick and easy function to do this
- give them as arguments to the template when rendering it
- have some HTML to display them in the templates
- remove the flash messages from the session.

## Getting started

If you didn't follow the tutorial, quickload those libraries:

```lisp
(ql:quickload '("hunchentoot" "djula" "easy-routes"))
```

We also introduce a local nickname, to shorten the use of `hunchentoot` to `ht`:

```lisp
(uiop:add-package-local-nickname :ht :hunchentoot)
```

Add this in your .lisp file if you didn't already, they
are typical for our web demos:

~~~lisp
(defparameter *port* 9876)
(defvar *server* nil "Our Hunchentoot acceptor")

(defun start (&key (port *port*))
  (format t "~&Starting the web server on port ~a~&" port)
  (force-output)
  (setf *server* (make-instance 'easy-routes:easy-routes-acceptor :port port))
  (ht:start *server*))

(defun stop ()
  (ht:stop *server*))
~~~


## Create flash messages in the session

This is our core function to quickly pile up a flash message to the web session.

The important bits are:

- we ensure to create a web session with `ht:start-session`.
- the `:flash` session object stores *a list* of flash messages.
- we decided that a flash messages holds those properties:
  - its type (string)
  - its message (string)


~~~lisp
(defun flash (type message)
  "Add a flash message in the session.

  TYPE: can be anything as you do what you want with it in the template.
     Here, it is a string that represents the Bulma CSS class for notifications: is-primary, is-warning etc.
  MESSAGE: string"
  (let* ((session (ht:start-session))               ;; <---- ensure we started a web session
         (flash (ht:session-value :flash session)))
    (setf (ht:session-value :flash session)
          ;; With a cons, REST returns 1 element
          ;; (when with a list, REST returns a list)
          (cons (cons type message) flash))))
~~~

Now, inside any route, we can call this function to add a flash message to the session:

```lisp
(flash "warning" "You are liking Lisp")
```

It's easy, it's handy, mission solved. Next.

## Delete flash messages when they are rendered

For this, we use Hunchentoot's life cycle and CLOS-orientation:

```lisp
;; delete flash after it is used.
;; thanks to https://github.com/rudolfochrist/booker/blob/main/app/controllers.lisp for the tip.
(defmethod ht:handle-request :after (acceptor request)
  (ht:delete-session-value :flash))
```

which means: after we have handled the current request, delete the `:flash` object from the session.

## Render flash messages in templates

### Set up Djula templates

Create a new `flash-template.html` file.

~~~lisp
(djula:add-template-directory "./")
(defparameter *flash-template* (djula:compile-template* "flash-template.html"))
~~~

{{% notice info %}}

You might need to change the current working
directory of your Lisp REPL to the directory of your .lisp file, so
that `djula:compile-template*` can find your template. Use the short
command `,cd` or `(swank:set-default-directory "/home/you/path/to/app/")`.
See also `asdf:system-relative-pathname system directory`.

{{% /notice %}}

### HTML template

This is our template. We use [Bulma
CSS](https://bulma.io/documentation/elements/notification/) to pimp it
up and to use its notification blocks.

```html
<!DOCTYPE html>
<html>

    <head>
      <meta charset="utf-8">
      <meta http-equiv="X-UA-Compatible" content="IE=edge">
      <meta name="viewport" content="width=device-width, initial-scale=1">
      <title>WALK - flash messages</title>
      <!-- Bulma Version 1-->
      <link rel="stylesheet" href="https://unpkg.com/bulma@1.0.2/css/bulma.min.css" />
    </head>

    <body>
      <!-- START NAV -->
      <nav class="navbar is-white">
        <div class="container">
          <div class="navbar-brand">
            <a class="navbar-item brand-text" href="#">
              Bulma Admin
            </a>
            <div class="navbar-burger burger" data-target="navMenu">
              <span></span>
            </div>
          </div>
          <div id="navMenu" class="navbar-menu">
            <div class="navbar-start">
              <a class="navbar-item" href="#">
                Home
              </a>
              <a class="navbar-item" href="#">
                Orders
              </a>
            </div>
          </div>
        </div>
      </nav>
      <!-- END NAV -->

      <div class="container">
        <div class="columns">
          <div class="column is-6">

            <h3 class="title is-4"> Flash messages. </h3>

            <div> Click <a href="/tryflash/">/tryflash/</a> to access an URL that creates a flash message and redirects you here.</div>

            {% for flash in flashes %}

            <div class="notification {{ flash.first }}">
              <button class="delete"></button>
            {{ flash.rest }}
            </div>

            {% endfor %}

          </div>
        </div>
      </div>

    </body>

    <script>
      // JS snippet to click the delete button of the notifications.
      // see https://bulma.io/documentation/elements/notification/
      document.addEventListener('DOMContentLoaded', () => {
        (document.querySelectorAll('.notification .delete') || []).forEach(($delete) => {
          const $notification = $delete.parentNode;

          $delete.addEventListener('click', () => {
            $notification.parentNode.removeChild($notification);
          });
        });
      });
    </script>

</html>
```

Look at

```
{% for flash in flashes %}
```

where we render our flash messages.

Djula allows us to write `{{ flash.first }}` and `{{ flash.rest }}` to
call the Lisp functions on those objects.

We must now create a route that renders our template.


## Routes

The `/flash/` URL is the demo endpoint:

```lisp
(easy-routes:defroute flash-route ("/flash/" :method :get) ()
  (djula:render-template*  *flash-template* nil
                           :flashes (or (ht:session-value :flash)
                                        (list (cons "is-primary" "No more flash messages were found in the session. This is a default notification.")))))
```

It is here that we pass the flash messages as a parameter to the template.

In your application, you must add this parameter in all the existing
routes.  To make this easier, you can:
- use Djula's [default template variables](https://mmontone.github.io/djula/djula/Variables.html#Default-template-variables), but our parameters are to be found dynamically in the current request's session, so we can instead
- create a "render" function of ours that calls `djula:render-template*` and always adds the `:flash` parameter. Use `apply`:

~~~lisp
(defun render (template &rest args)
  (apply
   #'djula:render-template* template nil
   ;; All arguments must be in a list.
   (list*
    :flashes (or (ht:session-value :flash)
                 (list (cons "is-primary" "No more flash messages were found in the session. This is a default notification.")))
    args)))
~~~

Finally, this is the route that creates a flash message:

```lisp
(easy-routes:defroute flash-redirect-route ("/tryflash/") ()
  (flash "is-warning" "This is a warning message held in the session. It should appear only once: reload this page and you won't see the flash message again.")
  (ht:redirect "/flash/"))
```

## Demo

Start the app with `(start)` if you didn't start Hunchentoot already,
otherwise it was enough to compile the new routes.

You should see a default notification. Click the "/tryflash/" URL and
you'll see a flash message, that is deleted after use.

Refresh the page, and you won't see the flash message again.

- full code: https://github.com/web-apps-in-lisp/web-apps-in-lisp.github.io/blob/master/content/building-blocks/flash-messages.lisp
