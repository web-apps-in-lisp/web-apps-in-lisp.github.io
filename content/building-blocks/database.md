+++
title = "Connecting to a database"
weight = 120
+++


Let's study different use cases:

- do you have an existing database you want to read data from?
- do you want to create a new database?
  - do you prefer CLOS orientation
  - or to write SQL queries?

We have many libraries to work with databases, let's have a recap first.

<!-- ## Database libraries -->

The [#database section on the awesome-cl list](https://github.com/CodyReichert/awesome-cl#database)
is a resource listing popular libraries to work with different kind of
databases. We can group them roughly in those categories:

- wrappers to one database engine (cl-sqlite, postmodern, cl-redis, cl-duckdb…),
- ORMs (Mito),
- interfaces to several DB engines (cl-dbi, cl-yesql…),
- lispy SQL syntax (sxql…)
- in-memory persistent object databases (bknr.datastore, cl-prevalence,…),
- graph databases in pure Lisp (AllegroGraph, vivace-graph) or wrappers (neo4cl),
- object stores (cl-store, cl-naive-store…)
- and other tools (pgloader, [which was re-written from Python to Common Lisp](https://tapoueh.org/blog/2014/05/why-is-pgloader-so-much-faster/)).

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Connect](#connect)
- [Run queries](#run-queries)
- [Insert rows](#insert-rows)
- [User-level API](#user-level-api)
- [Close connections](#close-connections)
- [The Mito ORM](#the-mito-orm)
- [How to integrate the databases into the web frameworks](#how-to-integrate-the-databases-into-the-web-frameworks)
- [Bonus: pimp your SQLite](#bonus-pimp-your-sqlite)
- [References](#references)

<!-- markdown-toc end -->


## How to query an existing database

Let's say you have a database called `db.db` and you want to extract
data from it.

For our example, quickload this library:

```lisp
(ql:quickload "cl-dbi")
```

[cl-dbi](https://github.com/fukamachi/cl-dbi/) can connect to major
database engines: PostGres, SQLite, MySQL.

Once `cl-dbi` is loaded, you can access its functions with the `dbi`
package prefix.

### Connect

To connect to a database, use `dbi:connect` with paramaters the DB type, and its name:

```lisp
(defparameter *db-name* "db.db")

(defvar *connection* nil "the DB connection")

(defun connect ()
  (if (uiop:file-exists-p *db-name*)
      (setf *connection* (dbi:connect :sqlite3 :database-name (get-db-name)))
      (format t "The DB file ~a does not exist." *db-name*)))
```

The available DB drivers are:
- `:mysql`
- `:sqlite3`
- `:postgres`

For the username and password, use the key arguments `:username` and `:password`.

When you connect for the first time, cl-dbi will automatically
quickload another dependency, depending on the driver. We advise to
add the relevant one to your list of dependencies in your .asd file
(or your binary will chok on a machine without Quicklisp, we learned
this the hard way).

    :dbd-sqlite3
    :dbd-mysql
    :dbd-postgres

We can now run queries.

### Run queries

Running a query is done is 3 steps:

- write the SQL query (in a string, with a lispy syntax…)
- `dbi:prepare` the query on a DB connection
- `dbi:execute` it
- and `dbi:fetch-all` results.


```lisp
(defparameter *select-products* "SELECT * FROM products LIMIT 100")

(dbi:fetch-all (dbi:execute (dbi:prepare *connection* *select-products*)))
```

This returns something like:

```
((:|id| 1 :|title| "Lisp Cookbook" :|shelf_id| 1 :|tags_id| NIL :|cover_url|
  "https://lispcookbook.github.io/cl-cookbook/orly-cover.png"
  :|created_at| "2024-11-07 22:49:23.972522Z" :|updated_at|
  "2024-12-30 20:55:51.044704Z")
 (:|id| 2 :|title| "Common Lisp Recipes" :|shelf_id| 1 :|tags_id| NIL
  :|cover_url| ""
  :|created_at| "2024-12-09 19:37:30.057172Z" :|updated_at|
  "2024-12-09 19:37:30.057172Z"))
```

We got a list of records where each record is a *property list*, a
list alternating a key (as a keyword) and a value.

Note how the keywords respect the case of our database fields with the `:|id|` notation.

With arguments, use a `?` placeholder in your SQL query and give a
list of arguments to `dbi:execute`:

~~~lisp
(defparameter *select-products* "SELECT * FROM products WHERE flag = ? OR updated_at > ?")

(let* ((query (dbi:prepare *connection* *select-products*))
       (query (dbi:execute query (list 0 "1984-01-01"))))  ;; <--- list of arguments
  (loop for row = (dbi:fetch query)
        while row
        ;; process "row".
        ))
~~~

### Insert rows

*(straight from cl-dbi's documentation)*

`dbi:do-sql` prepares and executes a single statement. It returns the
number of rows affected. It's typically used for non-`SELECT`
statements.

```lisp
(dbi:do-sql *connection*
            "INSERT INTO somewhere (flag, updated_at) VALUES (?, NOW())"
            (list 0))
```


### User-level API

`dbi` offers more functions to fetch results than `fetch-all`.

You can use `fetch` to get one result at a time or again `do-sql` to run any
SQL statement.


* connect [driver-name &amp; params] =&gt; &lt;dbi-connection&gt;
* connect-cached [driver-name &amp; params] =&gt; &lt;dbi-connection&gt;
* disconnect [&lt;dbi-connection&gt;] =&gt; T or NIL
* prepare [conn sql] =&gt; &lt;dbi-query&gt;
* prepare-cached [conn sql] =&gt; &lt;dbi-query&gt;
* execute [query &amp;optional params] =&gt; something
* fetch [result] =&gt; a row data as plist
* fetch-all [result] =&gt; a list of all row data
* do-sql [conn sql &amp;optional params]
* list-all-drivers [] =&gt; (&lt;dbi-driver&gt; ..)
* find-driver [driver-name] =&gt; &lt;dbi-driver&gt;
* with-transaction [conn]
* begin-transaction [conn]
* commit [conn]
* rollback [conn]
* ping [conn] =&gt; T or NIL
* row-count [conn] =&gt; a number of rows modified by the last executed INSERT/UPDATE/DELETE
* with-connection [connection-variable-name &body body]

### Close connections

You should take care of closing the DB connection.

`dbi` has a macro for that:

```lisp
(dbi:with-connection (conn :sqlite3 :database-name "/home/fukamachi/test.db")
  (let* ((query (dbi:prepare conn "SELECT * FROM People"))
         (query (dbi:execute query)))
    (loop for row = (dbi:fetch query)
          while row
          do (format t "~A~%" row))))
```

Inside this macro, `conn` binds to the current connection.

There is more but enough, please refer to cl-dbi's README.


## The Mito ORM

The [Mito ORM](https://github.com/fukamachi/mito/) provides a nice
object-oriented way to define schemas and query the database.

It supports SQLite3, PostgreSQL and MySQL, it has automatic
migrations, db schema versioning, and more features.

For example, this is how one can define a `user` table with two columns:

```lisp
(mito:deftable user ()
  ((name :col-type (:varchar 64))
   (email :col-type (or (:varchar 128) :null))))
```

Once we create the table, we can create and insert `user` rows with
methods such as `create-dao`:

```lisp
(mito:create-dao 'user :name "Eitaro Fukamachi" :email "e.arrows@gmail.com")
```

Once we edit the table definition (aka the class definition), Mito
will (by default) automatically migrate it.

There is much more to say, but we refer you to Mito's good
documentation and to the Cookbook.

## How to integrate the databases into the web frameworks

The web frameworks / web servers we use in this guide do not need
anything special. Just use a DB driver and fetch results in your
routes.

Using a DB connection per request with `dbi:with-connection` is a good idea.

## Bonus: pimp your SQLite

SQLite is a great database. It loves backward compatibility. As such,
its default settings may not be optimal for a web application seeing
some load. You might want to set some [PRAGMA
statements](https://www.sqlite.org/pragma.html) (SQLite settings).

To set them, look at your DB driver how to run a raw SQL query.

With `cl-dbi`, this would be `dbi:do-sql`:

```lisp
(dbi:do-sql *connection* "PRAGMA cache_size = -20000;")
```

Here's a nice list of pragmas useful for web development:

- [https://briandouglas.ie/sqlite-defaults/](https://briandouglas.ie/sqlite-defaults/)


## References

- [CL Cookbook#databases](https://lispcookbook.github.io/cl-cookbook/databases.html)
- [Mito](https://github.com/fukamachi/mito/)
- [cl-dbi](https://github.com/fukamachi/cl-dbi/)
