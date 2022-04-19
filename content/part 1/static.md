+++
title = "Static assets"
weight = 16
+++


## Hunchentoot

With Hunchentoot, use `create-folder-dispatcher-and-handler prefix directory`.

For example:

~~~lisp
(defun serve-static-assets ()
  "Let Hunchentoot serve static assets under the /src/static/ directory of your :myproject system.
  Reference static assets with the /static/ URL prefix."
  (push (hunchentoot:create-folder-dispatcher-and-handler
       "/static/" (merge-pathnames "src/static"  ;; starts without a /
                                   (asdf:system-source-directory :myproject))) ;; <- myproject
      hunchentoot:*dispatch-table*))

(serve-static-assets)
~~~

Now our project's static files located under
`/path/to/myproject/src/static/` are served with the `/static/` prefix:

```html
<img src="/static/img/banner.jpg" />
```
