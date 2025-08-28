+++
title = "PUT and request parameters"
weight = 145
+++

To access the body parameters of a PUT request, one must add `:PUT` to
`hunchentoot:*methods-for-post-parameters*`, which defaults to only
`(:POST)`:

```lisp
(push :put hunchentoot:*methods-for-post-parameters*)
```

This parameter:

> is a list of the request method types (as keywords) for which Hunchentoot will try to compute POST-PARAMETERS.

No such setting is required with Lack and Ningle.
