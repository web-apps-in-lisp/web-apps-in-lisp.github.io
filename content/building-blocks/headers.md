
+++
title = "Headers"
weight = 40
+++

A quick reference.

These functions have to be used in the context of a web request.


## Set headers

Use `header-out` to set headers, like this:

~~~lisp
(setf (hunchentoot:header-out "HX-Trigger") "myEvent")
~~~

This sets the header of the current request.

USe `headers-out` (plural) to get an association list of headers:

> An alist of the outgoing http headers not including the 'Set-Cookie', 'Content-Length', and 'Content-Type' headers. Use the functions HEADER-OUT and (SETF HEADER-OUT) to modify this slot.

## Get headers

Use the `header-in*` and `headers-in*` (plural) function:

    Function: (header-in* name &optional (request *request*))

> Returns the incoming header with name NAME. NAME can be a keyword (recommended) or a string.

`headers-in*`:

    Function: (headers-in* &optional (request *request*))

> Returns an alist of the incoming headers associated with the REQUEST object REQUEST.

## Reference

Find some more here:

- [https://common-lisp-libraries.readthedocs.io/hunchentoot/#headers-in_1](https://common-lisp-libraries.readthedocs.io/hunchentoot/#headers-in_1)
