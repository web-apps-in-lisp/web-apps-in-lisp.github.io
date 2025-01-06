+++
title = "Electron"
weight = 290
+++

Electron is heavy, but really cross-platform, and it has many tools
around it. It allows to build releases for the three major OS from
your development machine, its ecosystem has tools to handle updates,
etc.

Advise: study it before discarding it.

It isn't however the only portable web view solution. See our next section.

{{% notice info %}}
This page appeared first on [lisp-journey: three web views for Common Lisp, cross-platform GUIs](https://lisp-journey.gitlab.io/blog/three-web-views-for-common-lisp--cross-platform-guis/).
{{% /notice %}}

## Ceramic (old but works)

[Ceramic](https://github.com/ceramic/ceramic/) is a set of utilities
around Electron to help you build an Electron app: download the npm
packages, open a browser window, etc.

Here's its getting started snippet:

```lisp
;; Start the underlying Electron process
(ceramic:start)
;; ^^^^^ this here downloads ±200MB of node packages under the hood.

;; Create a browser window
(defvar window (ceramic:make-window :url "https://www.google.com/"
                                    :width 800
                                    :height 600))

;; Show it
(ceramic:show window)
```

When you run `(ceramic:bundle :ceramic-hello-world)` you get a .tar
file with your application, which you can distribute. Awesome!

But what if you don't want to redirect to google.com but open your own
app? You just build your web app in CL, run the webserver
(Hunchentoot, Clack…) on a given port, and you'll open
`localhost:[PORT]` in Ceramic/Electron. That's it.

Ceramic wasn't updated in five years as of date and it downloads an
outdated version of Electron by default (see `(defparameter
*electron-version* "5.0.2")`), but you can change the version yourself.

The new [Neomacs project, a structural editor and web browser](https://github.com/neomacs-project/neomacs/), is a great modern example on how to use Ceramic. Give it a look and give it a try!

What Ceramic actually does is abstracted away in the CL functions, so
I think it isn't the best to start with. We can do without it to
understand the full process, here's how.

- Ceramic API reference: http://ceramic.github.io/docs/api-reference.html

## Electron from scratch

Here's our web app embedded in Electron:

![](https://media2.dev.to/cdn-cgi/image/width=800%2Cheight=%2Cfit=scale-down%2Cgravity=auto%2Cformat=auto/https%3A%2F%2Fdev-to-uploads.s3.amazonaws.com%2Fuploads%2Farticles%2Flqjy44zgpae1jp5vkxwx.png "Our web app running on Electron")

Our steps are the following:

- follow the Electron installation instructions,
- build a binary of your Lisp web app, including assets and HTML templates, if any.
  * see this post: https://lisp-journey.gitlab.io/blog/lisp-for-the-web-build-standalone-binaries-foreign-libraries-templates-static-assets/ (the process will be a tad simpler without Djula templates)
- bundle this binary into the final Electron build.
- and that's it.

You can also run the Lisp web app from sources, of course, without
building a binary, but then you'll have to ship all the lisp sources.

<!-- You can also have a look at -->
<!-- https://github.com/mikelevins/electron-lisp-boilerplate for this, -->
<!-- their main.js has the pattern, using child_process. -->

### main.js

The most important file to an Electron app is the main.js. The one we show below does the following:

- it starts Electron
- it starts our web application on the side, as a child process, from a binary name, and a port.
- it shows our child process' stdout and stderr
- it opens a browser window to show our app, running on localhost.
- it handles the close event.

Here's our version.

```javascript
console.log(`Hello from Electron 👋`)

const { app, BrowserWindow } = require('electron')

const { spawn } = require('child_process');

// FIXME Suppose we have our app binary at the current directory.

// FIXME This is our hard-coded binary name.
var binaryPaths = [
    "./openbookstore",
];

// FIXME Define any arg required for the binary.
// This is very specific to the one I built for the example.
var binaryArgs = ["--web"];

const binaryapp = null;

const runLocalApp = () => {
    "Run our binary app locally."
    console.log("running our app locally…");
    const binaryapp = spawn(binaryPaths[0], binaryArgs);
    return binaryapp;
}

// Start an Electron window.

const createWindow = () => {
  const win = new BrowserWindow({
    width: 800,
    height: 600,
  })

  // Open localhost on the app's port.
  // TODO: we should read the port from an environment variable or a config file.
  // FIXME hard-coded PORT number.
  win.loadURL('http://localhost:4242/')
}

// Run our app.
let child = runLocalApp();

// We want to see stdout and stderr of the child process
// (to see our Lisp app output).
child.stdout.on('data', (data) => {
  console.log(`stdout:\n${data}`);
});

child.stderr.on('data', (data) => {
  console.error(`stderr: ${data}`);
});

child.on('error', (error) => {
  console.error(`error: ${error.message}`);
});

// Handle Electron close events.
child.on('close', (code) => {
  console.log(`openbookstore process exited with code ${code}`);
});

// Open it in Electron.
app.whenReady().then(() => {
    createWindow();

    // Open a window if none are open (macOS)
    if (process.platform == 'darwin') {
        app.on('activate', () => {
            if (BrowserWindow.getAllWindows().length === 0) createWindow()
        })
    }
})


// On Linux and Windows, quit the app main all windows are closed.
app.on('window-all-closed', () => {
    if (process.platform !== 'darwin') {
        app.quit();
    }
})
```

Run it with `npm run start` (you also have an appropriate packages.json), this gets you the previous screenshot.

JS and Electron experts, please criticize and build on it.

**Missing parts**

We didn't fully finish the example: we need to automatically bundle
the binary into the Electron release.

Then, if you want to communicate from the Lisp app to the Electron
window, and the other way around, you'll have to use the JavaScript layers. Ceramic might help here.

## What about Tauri?

Bundling an app with [Tauri](https://tauri.app/) will, AFAIK (I just
tried quickly), involve the same steps than with Electron. Tauri might
still have less tools for it. You need the Rust toolchain.
