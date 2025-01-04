+++
title = "User log-in"
weight = 125
+++

How do you check if a user is logged-in, and how do you do the actual log in?

We show an example, without handling passwords yet. See the next section for passwords.

We'll build a simple log-in page to an `admin/` private dashboard.

![](/building-blocks/login.png)

![](/building-blocks/logged-in.png)

What do we need to do exactly?

- we need a function to get a user by its ID
- we need a function to check that a password is correct for a given user
- we need two templates:
  - a login template
  - a template for a logged-in user
- we need a route and we need to handle a POST request
  - when the log-in is successful, we need to store a user ID in a web session

We choose to structure our app with an `admin/` URL that will show
both the login page or the "dashboard" for logged-in users.

<!-- - when we don't find a user ID in the session, we render the log-in page -->

Let's start with the model functions.

We use these libraries:

```lisp
(ql:quickload '("hunchentoot" "djula" "easy-routes"))
```

We still work from inside our `:myproject` package. You should have
this at the top of your file:

```lisp
(in-package :myproject)
```


## Get users

```lisp
(defun get-user (name)
  (list :name name :password "demo")) ;; <--- all our passwords are "demo"
```

Yes indeed, that's a dummy function. You will add your own logic
later, we focus on the web stack. Here we return a user object, a
*plist* with a name and a password. So to speak.

~~~lisp
(defun valid-user-p (name password)
  (let ((user (get-user name)))
    (and user
         (string= name (getf user :name))
         (string= password (getf user :password)))))
~~~

## Templates: login, welcome

For convenience we again define our templates as strings.

```lisp
;;; Templates.
;;; XXX: we have to escape the quotes in our string templates. When they are in files we don't.
(defparameter *template-login* "
  <html lang=en>
   <head>
    <meta charset=UTF-8>
    <title>Login</title>
   </head>
   <body>
    <div>
     Login form.
    </div>
    <div>
     Any user name is valid. The password is \"demo\".
    </div>

    {% if error %}
    <p style=\"color: red;\">Invalid username or password</p>
    {% endif %}

    <form method=post action=\"/admin/\">
     <p>Username:
      {% if name %}
      <input type=text name=name value=\"{{ name }}\">
      {% else %}
      <input type=text name=name>
      {% endif %}
     <p>Password:
      <input type=password name=password>
     <p>
      <input type=submit value=\"Log In\">
    </form>
   </body>
  </html> "
  )

(defparameter *template-welcome* "
  <html lang=en>
   <head>
    <meta charset=UTF-8>
    <title>Welcome</title>
   </head>
   <body>
    <h1>Welcome, {{ name }}!</h1>
    <div>You are logged in to your admin dashboard.</div>
    <a href=\"/admin/logout\">Log out</a>
   </body>
  </html>
  ")
```

Please refer to the demo to understand how we use them (and their
shortcomings). We need the `render` function (see also the full code
below).

## Views: are we logged in?

You can start with this route:

```lisp
(defun loggedin-p ()
  (hunchentoot:session-value 'name))

;; GET
(easy-routes:defroute admin-route ("/admin/" :method :get) ()
  (if (loggedin-p)
    (render *template-welcome* :name (hunchentoot:session-value 'name))
    (render *template-login*))
```

We are simply querying the session for the user name. If it's present,
that means we have established it at login.

Now is a great time to use easy-routes' "decorators".

We can shorten the route to this:

```lisp
(defun @auth (next)
  (log:info "checking session")
  (if (loggedin-p)
      (funcall next)
      (render *template-login*)))

;; GET
(easy-routes:defroute admin-route ("/admin/" :method :get :decorators ((@auth))) ()
  (render *template-welcome* :name (hunchentoot:session-value 'name)))
```

Yes, `((@auth))` is between 2 `(( ))` because that will be useful. We
can call "decorators" with arguments.

The two routes are strictly equivalent, but the second one allows to
offload and refactor logic to other functions.

## First test

Please see the tutorial for how to start a web server.

If you compiled the routes while a connection to a web server is
active, then your route is accessible.

Visit [http://localhost:8899/admin/](http://localhost:8899/admin/), you should see the login form.

We didn't handle the POST request yet.

## login: POST request

```lisp
;; POST
(easy-routes:defroute admin-route/POST ("/admin/" :method :post) (name password)
  (cond
    ((valid-user-p name password)
     (hunchentoot:start-session)
     (setf (hunchentoot:session-value 'name) name)
     (render *template-welcome* :name name))
    (t
     (render *template-login* :name name :error t))))
```

Beware of this gotcha: the route names must be unique. Otherwise, you
will override your previous route definition. We name it
`admin-route/POST`.

Our login HTML defines two inputs:

    <input type=text name=name>
    <input type=text name=password>

that's why we declared those as POST
parameters in the route with `(name password)`.

Our `valid-user-p` function only checks that the password equals "demo".

Depending on the result, we display the login page again, with an
error message, or we display our welcome page and right before we do
these important steps:

- we start a session
- and we store our user ID

We are logged in o/

## Logout

Notice the logout button in the welcome page.

Let's define the logout route:

```lisp
(hunchentoot:define-easy-handler (logout :uri "/admin/logout") ()
  (hunchentoot:delete-session-value 'name)
  (hunchentoot:redirect "/admin/"))
```

We have to delete our user's ID! That's the step not to forget.

We could also delete the current session object altogether with:

```lisp
(hunchentoot:remove-session (hunchentoot:*session*))
```

that depends if you stored more data. The `*session*` "global" object
is the session in the context of the current request.

At last we redirect to the `admin/` URL, which is going to check the user
ID in the session, which isn't present anymore, and thus show the
login form.

And we've gone circle.

## Redirect and generate an URL by name

We just used a redirect.

You will notice that the routes `/admin` and `/admin/` are
different. We set up a quick redirect.

```lisp
(hunchentoot:define-easy-handler (admin2 :uri "/admin") ()
  (hunchentoot:redirect "/admin/"))
```

but wait, did we copy an URL by name? We can instead use

```lisp
(easy-routes:genurl 'admin-route)
;; "/admin/"
```

We also have `genurl*` to generate an absolute URL:

~~~lisp
(easy-routes:genurl* 'admin-route)
;; "http://localhost/admin/"
~~~

These functions accept arguments to set the PATH and URL parameters.


## Hunchentoot code

This is the equivalent Hunchentoot route:

```lisp
(hunchentoot:define-easy-handler (admin :uri "/dashboard/") (name password)
    (ecase (hunchentoot:request-method*)
      (:get
       (if (loggedin-p)
           (render *template-welcome*)
           (render *template-login*)))
      (:post
       (cond
         ((valid-user-p name password)
          (hunchentoot:start-session)
          (setf (hunchentoot:session-value 'name) name)
          (render *template-welcome* :name name))
         (t
          (render *template-login* :name name :error t))))
      ))
```

Remarks:

- we can't dispatch on the request type, so we use the `ecase` on `request-method*`
- we can't use "decorators" so if use branching
- it isn't very clear but `name` and `password` are only used in the POST part.
  - we can also use `(hunchentoot:post-parameter "name")` (the parameter as a string)
- otherwise, it's pretty similar.

## Full code

```lisp
(in-package :myproject)

;; User-facing paramaters.
(defparameter *port* 8899)

;; Internal variables.
(defvar *server* nil)


;;; Models.
(defun get-user (name)
  (list :name name :password "demo")) ;; <--- all our passwords are "demo"

(defun valid-user-p (name password)
  (let ((user (get-user name)))
    (and user
         (string= name (getf user :name))
         (string= password (getf user :password)))))

;;; Templates.
;;; XXX: we have to escape the quotes in our string templates. When they are in files we don't.
(defparameter *template-login* "
  <html lang=en>
   <head>
    <meta charset=UTF-8>
    <title>Login</title>
   </head>
   <body>
    <div>
     Login form.
    </div>
    <div>
     Any user name is valid. The password is \"demo\".
    </div>

    {% if error %}
    <p style=\"color: red;\">Invalid username or password</p>
    {% endif %}

    <form method=post action=\"/admin/\">
     <p>Username:
      {% if name %}
      <input type=text name=name value=\"{{ name }}\">
      {% else %}
      <input type=text name=name>
      {% endif %}
     <p>Password:
      <input type=password name=password>
     <p>
      <input type=submit value=\"Log In\">
    </form>
   </body>
  </html> "
  )

(defparameter *template-welcome* "
  <html lang=en>
   <head>
    <meta charset=UTF-8>
    <title>Welcome</title>
   </head>
   <body>
    <h1>Welcome, {{ name }}!</h1>
    <div>You are logged in to your admin dashboard.</div>
    <a href=\"/admin/logout\">Log out</a>
   </body>
  </html>
  ")

(defun render (template &rest args)
  (apply
   #'djula:render-template*
   (djula:compile-string template)
   nil
   args))


;; Views.
(defun loggedin-p ()
  (hunchentoot:session-value 'name))

(defun @auth (next)
  (if (loggedin-p)
      (funcall next)
      (render *template-login*)))

;; GET
(easy-routes:defroute admin-route ("/admin/" :method :get :decorators ((@auth))) ()
  (render *template-welcome* :name (hunchentoot:session-value 'name)))

;; POST
(easy-routes:defroute admin-route/POST ("/admin/" :method :post) (name password)
  (cond
    ((valid-user-p name password)
     (hunchentoot:start-session)
     (setf (hunchentoot:session-value 'name) name)
     (render *template-welcome* :name name))
    (t
     (render *template-login* :name name :error t))))

(hunchentoot:define-easy-handler (logout :uri "/admin/logout") ()
  (hunchentoot:delete-session-value 'name)
  (hunchentoot:redirect (easy-routes:genurl 'admin-route)))

;; Server.
(defun start-server (&key (port *port*))
  (format t "~&Starting the login demo on port ~a~&" port)
  (unless *server*
    (setf *server* (make-instance 'hunchentoot:easy-acceptor :port port)))
  (hunchentoot:start *server*))

(defun stop-server ()
  (hunchentoot:stop *server*))
```
