+++
title = "Users and passwords"
weight = 130
+++

We don't know of a Common Lisp framework that will create users and
roles for you and protect your routes all at the same time. We have
building blocks but you'll have to either write some glue Lisp code.

You can also turn to an external tool (such as Keycloak) that will
provide all the industrial-grade user management.

If you like the Mito ORM, look at [mito-auth](https://github.com/fukamachi/mito-auth/) and [mito-email-auth](https://github.com/40ants/mito-email-auth).


## Creating users

If you use a database, you'll have to create at least a `users`
table. It would typically define:
- a unique ID (integer, primary key)
- a name (varchar)
- an email (varchar)
- a password (varchar (and encrypted))
- optionally, a key to the table listing roles.

You can start with this:

~~~SQL
CREATE TABLE users (
  id INTEGER PRIMARY KEY,
  username VARCHAR(255),
  email VARCHAR(255),
  password VARCHAR(255),
)
~~~

You can run this right now with SQLite on the command line:

```
$ sqlite3 db.db "CREATE TABLE users (id INTEGER PRIMARY KEY, username VARCHAR(255), email VARCHAR(255), password VARCHAR(255))"
```

This creates the database if it doesn't exist. SQLite reads SQL from the command line.

Create users:

```
$ sqlite3 db.db "INSERT INTO users VALUES(1,'Alice','alice@mail','xxx');"
```

Did it work? Run `SELECT * FROM users;`.


## Encrypting passwords

### With cl-bcrypt

[cl-bcrypt](https://github.com/dnaeon/cl-bcrypt) is a password hashing and verification library. It is as simple to use as this:

```lisp
CL-USER> (defparameter *password*
           (bcrypt:make-password "my-secret-password"))
*PASSWORD*
```

and you can specify another salt, another cost factor and another algorithm identifier.

Then you can use `bcrypt:encode` to get a string reprentation of the password:

~~~lisp
CL-USER> (bcrypt:encode *password*)
"$2a$16$ClVzMvzfNyhFA94iLDdToOVeApbDppFru3JXNUyi1y1x6MkO0KzZa"
~~~

and you decode a password with `decode`.


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

## See also

* [cl-authentic](https://github.com/charJe/cl-authentic) -  Password management for Common Lisp (web) applications. [LLGPL][8].
  - safe password storage: cleartext-free, using your choice of hash algorithm through ironclad, storage in an SQL database,
  - password reset mechanism with one-time tokens (suitable for mailing to users for confirmation),
  - user creation optionally with confirmation tokens (suitable for mailing to users),

and more on the awesome-cl list.
