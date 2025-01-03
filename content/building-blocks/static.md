+++
title = "Static assets"
weight = 15
+++

How can we serve static assets?

Remember from [simple web server](/building-blocks/simple-web-server/)
how Hunchentoot serves files from a `www/` directory by default. We can change that.

## Hunchentoot

Use the `create-folder-dispatcher-and-handler prefix directory` function.

For example:

~~~lisp
(defun serve-static-assets ()
  "Let Hunchentoot serve static assets under the /src/static/ directory
  of your :myproject system.
  Then reference static assets with the /static/ URL prefix."
  (push (hunchentoot:create-folder-dispatcher-and-handler
         "/static/"
         (merge-pathnames "src/static" ;; starts without a /
                          (asdf:system-source-directory :myproject))) ;; <- myproject
        hunchentoot:*dispatch-table*))
~~~

and call it in the function that starts your application:

```lisp
(serve-static-assets)
```

Now our project's static files located under `src/static/` are served
with the `/static/` prefix, access them like this:

```html
<img src="/static/img/banner.jpg" />
```
