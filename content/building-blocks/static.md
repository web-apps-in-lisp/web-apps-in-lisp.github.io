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
          (asdf:system-relative-pathname :myproject "src/static/"))
          ;;                                        ^^^ starts without a /
        hunchentoot:*dispatch-table*))
~~~

and call it in the function that starts your application:

```lisp
(serve-static-assets)
```

Now our project's static files located under `src/static/` are served
with the `/static/` prefix. Access them like this:

```html
<img src="/static/img/banner.jpg" />
```

or

```html
<script src="/static/test.js" type="text/javascript"></script>
```

where the file `src/static/test.js` could be

```js
console.log("hello");
```
