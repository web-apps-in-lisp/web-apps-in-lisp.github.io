+++
title = "Web views: cross-platform GUIs"
weight = 300
+++

Web views are lightweight and cross-platform. They are nowadays a good
solution to ship a GUI program to your users.

We present two: WebUI and Webview.h, through CLOG Frame.

{{% notice info %}}
This page appeared first on [lisp-journey: three web views for Common Lisp, cross-platform GUIs](https://lisp-journey.gitlab.io/blog/three-web-views-for-common-lisp--cross-platform-guis/).
{{% /notice %}}

## WebUI

[WebUI](https://github.com/webui-dev/webui/) is a new kid in town. It is in development, it has bugs. You can view it as a wrapper around a browser window (or webview.h).

However it is ligthweight, it is *easy to build* and we have Lisp bindings.

A few more words about it:

>  Use any web browser or WebView as GUI, with your preferred language in the backend and modern web technologies in the frontend, all in a lightweight portable library.

- written in pure C
- one header file
- multi-platform & multi-browser
- opens a real browser (you get the web development tools etc)
- cross-platform webview
- we can call JS from Common Lisp, and call Common Lisp from JS.

> Think of WebUI like a WebView controller, but instead of embedding the WebView controller in your program, which makes the final program big in size, and non-portable as it needs the WebView runtimes. Instead, by using WebUI, you use a tiny static/dynamic library to run any installed web browser and use it as GUI, which makes your program small, fast, and portable. All it needs is a web browser.

> your program will always run on all machines, as all it needs is an installed web browser.

Sounds compelling right?

The other good news is that Common Lisp was one of the first languages
it got bindings for. How it happened: I was chating in Discord, mentioned WebUI and BAM! @garlic0x1 developed bindings:

- https://github.com/garlic0x1/cl-webui/

thank you so much! (@garlic0x1 has more cool projects on GitHub you can browse. He's also a contributor to Lem)

Here's a simple snippet:


```lisp
(defpackage :webui/examples/minimal
  (:use :cl :webui)
  (:export :run))
(in-package :webui/examples/minimal)

(defun run ()
  (let ((w (webui-new-window)))
    (webui-show w "<html>Hello, world!</html>")
    (webui-wait)))
```

I would be the happiest lisper in the world if I didn't have an annoying issue. See [#1](https://github.com/garlic0x1/cl-webui/issues/1). I can run my example just fine, but nothing happens the second time :/ I don't know if it's a WebUI thing, the bindings, my system, my build of WebUI… so I'll give this more time.

Fortunately though, the third solution of this blog post is my favourite! o/

## CLOG Frame (webview.h for all)

[CLOG Frame](https://github.com/rabbibotton/clog/tree/main/clogframe)
is part of the CLOG framework. However, it is *not* tied to CLOG… nor
to Common Lisp!

CLOG Frame is a short C++ program that builds an executable that takes
an URL and a PORT as CLI parameters and opens a [webview.h](https://github.com/webview/webview) window.

It's easy to build and works just fine. It's a great approach.

Back to our matter.

This is CLOG Frame: 20 lines!

```Cpp
#include <iostream>
#include <sstream>
#include <string>
#include "webview.h"

int main(int argc,char* argv[]) {
  webview::webview w(true, nullptr);
  webview::webview *w2 = &w;
  w.set_title(argv[1]);
  w.set_size(std::stoi(argv[3]), std::stoi(argv[4]), WEBVIEW_HINT_NONE);
  w.bind("clogframe_quit", [w2](std::string s) -> std::string {
    w2->terminate();
    return "";
  });
  std::ostringstream o;
  o << "http://127.0.0.1:" << argv[2];
  w.navigate(o.str());
  w.run();
  return 0;
}
```

Compile it on GNU/Linux like this and don't you worry, it takes a fraction of a second:

    c++ clogframe.cpp -ldl `pkg-config --cflags --libs gtk+-3.0 webkit2gtk-4.0` -o clogframe

(see its repo for other platforms)

this gives you a `clogframe` binary. Put it in your $PATH or remember its location. It's just a short C++ binary, so it weights 197Kb.

Now, back to your web app that you wrote in Common Lisp and that is
waiting to be shipped to users.

Start your web app. Say it is started on port 4284.

From the Lisp side, open a CLOG Frame window like this


```lisp
(uiop:launch-program (list "./clogframe"
                           "Admin"
                           (format nil "~A/admin/" 4284)
                           ;; window dimensions (strings)
                           "1280" "840"))
```

and voilà.


![](https://lisp-journey.gitlab.io/images/clogframe-on-top-emacs.png "A CLOG Frame window showing a WIP Common Lisp web app on top of Emacs.")

Now for the cross-platform part, you'll need to build clogframe and
your web app on the target OS (like with any CL app). Webview.h is cross-platform.
Leave us a comment when you have a good CI setup for the three main OSes (I am studying [40ants/ci](https://github.com/40ants/ci/) and [make-common-lisp-program](https://github.com/melusina-org/make-common-lisp-program/) for now).
