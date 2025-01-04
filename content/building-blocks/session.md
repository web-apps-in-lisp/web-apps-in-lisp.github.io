+++
title = "Sessions"
weight = 35
+++

When a web client (a user's browser) connects to your app, you can
start a session with it, and store data, the time of the session.

Hunchentoot uses cookies to store the session ID, and if not possible
it rewrites URLs. So, at every subsequent request by this web client,
you can check if there is a session data.

You can store any Lisp object in a web session. You only have to:

- start a session with `(hunchentoot:start-session)`
  - for example, when a user logs in successfully
- store an object with `(setf (hunchentoot:session-value 'key) val)`
  - for example, store the username at the log-in
- and get the object with `(hunchentoot:session-value 'key)`.
  - for example, in any route where you want to check that a user is logged in. If you don't find a session key you want, you would redirect to the login page.

See our example in the next section about log-in.

## References

- [https://common-lisp-libraries.readthedocs.io/hunchentoot/#8-session](https://common-lisp-libraries.readthedocs.io/hunchentoot/#8-session)
