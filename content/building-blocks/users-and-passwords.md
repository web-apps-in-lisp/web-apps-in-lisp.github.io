+++
title = "Users and passwords"
weight = 130
+++

We don't know of a Common Lisp framework that will create users and
roles for you and protect your routes. You'll have to either write
some Lisp, either use an external tool (such as Keycloak) that will
provide all the user management.

{{% notice info %}}

Stay tuned! We are on to something.

{{% /notice %}}


## Creating users

If you use a database, you'll have to create at least to save users. A
`user` table typically defines:
- a unique ID (the primary key)
- a name (varchar)
- an email (varchar)
- a password (encrypted)
- optionally, a key to the table listing roles.


## Checking a user is logged-in

<!-- https://www.reddit.com/r/Common_Lisp/comments/1f7bfql/simple_session_management_with_hunchentoot/ -->

A framework provides a way to work with sessions.

Our web app defines routes. Routes are, in the end, only functions.

We must find a way to check for users before the route function. If
our check is successful (the user registered in the current session)
is logged-in, we call the next function (the route body). If our check
is falsy, we don't execute the route but we redirect to the login page
instead. Simple right?


### Hunchentoot and easy-routes

`easy-routes` provides us with an easy way to call any function before
the route body. Following the naming of a popular language, they are
called "decorators".

Remember the shape of our routes:

```lisp
(easy-routes:defroute root ("/") ()
    "hello app")
```

We add a list of decorators after the `"/"` part, like this:

```lisp
 (defroute my-protected-route ("/foo" :method :get
                                      :decorators ((@json)))
	()
	"hello decorated route")
```

but what is `@json`? It's a function:

```lisp
(defun @json (next)
  (setf (hunchentoot:content-type*) "application/json")
  (funcall next))
```

You can ignore the `@` sign, it doesn't mean anything in Common Lisp
(but as it's part of the function name it can help you reference all
your route decorators).

Route "decorators" must accept at least one argument: `next`, the
function that will be called next (so, at one point, our route body)
*if we want to*. Look at `(funcall next)`: our decorator correctly
calls it.

So what it is function doing? Keep it mind that it is called in the
context of a web request. So we can call `hunchentoot:content-type*`
(note the `*`, this function is applied on the current web
request). We are setting this request's content-type to
`application/json`.

Yes, you can copy-paste the `setf` line directly into your function.

Here's another "decorator" (straight from easy-routes' documentation):

```lisp
(defun @auth (next)
  (let ((*user* (hunchentoot:session-value 'user)))
    (if (not *user*)
	  (hunchentoot:redirect "/login")
	  (funcall next))))
```

Now that's interesting. It's doing this:
- it gets a value from the current web session. This can be any Lisp object.
- it binds it to a global variable (we suppose the application re-uses it later) (global variables are thread-local)
- if a user was registered in the session, we call the `next` method to run other decorators and the route body
- otherwise, we redirect to the login page.


### Caveman

In Caveman, `*session*` is a hash table that represents the session's
data. Here are our login and logout functions:

```lisp
(defun login (user)
  "Log the user into the session"
  (setf (gethash :user *session*) user))

(defun logout ()
  "Log the user out of the session."
  (setf (gethash :user *session*) nil))
```

We define a simple predicate:

```lisp
(defun logged-in-p ()
  (gethash :user cm:*session*))
```

We don't know a mechanism as easy-routes' "decorators" but we define a
`with-logged-in` macro:

```lisp
(defmacro with-logged-in (&body body)
  `(if (logged-in-p)
       (progn ,@body)
       (render #p"login.html"
               '(:message "Please log-in to access this page."))))
```

If the user isn't logged in, there will be nothing stored in the session store,
and we render the login page. When all is well, we execute the macro's
body. We use it like this:

```lisp
(defroute "/account/logout" ()
  "Show the log-out page, only if the user is logged in."
  (with-logged-in
    (logout)
    (render #p"logout.html")))

(defroute ("/account/review" :method :get) ()
  (with-logged-in
    (render #p"review.html"
            (list :review (get-review (gethash :user *session*))))))
```

and so on.


## Encrypting passwords

### With cl-pass

[cl-pass](https://github.com/eudoxia0/cl-pass) is a password hashing and verification library. It is as simple to use as this:

```lisp
(cl-pass:hash "test")
;; "PBKDF2$sha256:20000$5cf6ee792cdf05e1ba2b6325c41a5f10$19c7f2ccb3880716bf7cdf999b3ed99e07c7a8140bab37af2afdc28d8806e854"
(cl-pass:check-password "test" *)
;; t
(cl-pass:check-password "nope" **)
;; nil
```

You might also want to look at
[hermetic](https://github.com/eudoxia0/hermetic), a simple
authentication system for Clack-based applications.

### Manually (with Ironclad)

In this recipe we do the encryption and verification ourselves. We use the de-facto standard
[Ironclad](https://github.com/froydnj/ironclad) cryptographic toolkit
and the [Babel](https://github.com/cl-babel/babel) charset
encoding/decoding library.

The following snippet creates the password hash that should be stored in your
database. Note that Ironclad expects a byte-vector, not a string.

```lisp
(defun password-hash (password)
  (ironclad:pbkdf2-hash-password-to-combined-string
   (babel:string-to-octets password)))
```

`pbkdf2` is defined in [RFC2898](https://tools.ietf.org/html/rfc2898).
It uses a pseudorandom function to derive a secure encryption key
based on the password.

The following function checks if a user is active and verifies the
entered password. It returns the user-id if active and verified and
nil in all other cases even if an error occurs. Adapt it to your
application.

```lisp
(defun check-user-password (user password)
  (handler-case
      (let* ((data (my-get-user-data user))
             (hash (my-get-user-hash data))
             (active (my-get-user-active data)))
        (when (and active (ironclad:pbkdf2-check-password (babel:string-to-octets password)
                                                          hash))
          (my-get-user-id data)))
    (condition () nil)))
```

And the following is an example on how to set the password on the
database. Note that we use `(password-hash password)` to save the
password. The rest is specific to the web framework and to the DB
library.

```lisp
(defun set-password (user password)
  (with-connection (db)
    (execute
     (make-statement :update :web_user
                     (set= :hash (password-hash password))
                     (make-clause :where
                                  (make-op := (if (integerp user)
                                                  :id_user
                                                  :email)
                                           user))))))
```

*Credit: `/u/arvid` on [/r/learnlisp](https://www.reddit.com/r/learnlisp/comments/begcf9/can_someone_give_me_an_eli5_on_hiw_to_encrypt_and/)*.
