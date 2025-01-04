;; https://paste.sr.ht/%7Emarcuskammer/587dc97736e6ffc3d2b37895f73c36bb7ba9c0e7

;; (defpackage :login-demo
  ;; (:use :cl))

;; (in-package :login-demo)
(in-package :myproject)

;; User-facing paramaters.
(defparameter *port* 9001)

;; Internal variables.
(defvar *server* nil)


;;; Models.
(defun get-user (name)
  (list :name name :password "demo")) ;; <--- all our passwords are "demo"

(defun valid-user-p (name password)
  (let ((user (get-user name)))
    (and user
         (string= name (getf user :name))
         (string= password (getf user :password)))))

;;; Templates.
;;; XXX: we have to escape the quotes in our string templates.
(defparameter *template-login* "
  <html lang=en>
   <head>
    <meta charset=UTF-8>
    <title>Login</title>
   </head>
   <body>
    <div>
     Login form.
    </div>
    <div>
     Any user name is valid. The password is \"demo\".
    </div>

    {% if error %}
    <p style=\"color: red;\">Invalid username or password</p>
    {% endif %}

    <form method=post action=\"/admin/\">
     <p>Username:
      {% if name %}
      <input type=text name=name value=\"{{ name }}\">
      {% else %}
      <input type=text name=name>
      {% endif %}
     <p>Password:
      <input type=password name=password>
     <p>
      <input type=submit value=\"Log In\">
    </form>
   </body>
  </html> "
  )

(defparameter *template-welcome* "
  <html lang=en>
   <head>
    <meta charset=UTF-8>
    <title>Welcome</title>
   </head>
   <body>
    <h1>Welcome, {{ name }}!</h1>
    <div>You are logged in to your admin dashboard.</div>
    <a href=\"/admin/logout\">Log out</a>
   </body>
  </html>
  ")

(defun render (template &rest args)
  (apply
   #'djula:render-template*
   (djula:compile-string template)
   nil
   args))


;; Views.
(defun loggedin-p ()
  (hunchentoot:session-value 'name))

(defun @auth (next)
  (log:info "checking session")
  (if (loggedin-p)
      (funcall next)
      (render *template-login*)))

;; GET
(easy-routes:defroute admin-route ("/admin/" :method :get :decorators ((@auth))) ()
  (render *template-welcome* :name (hunchentoot:session-value 'name)))

;; using @auth is equivalent to doing a check in the route body:
#|
(if (loggedin-p)
  (render *template-welcome*)
  (render *template-login*))
|#

;; POST
(easy-routes:defroute admin-route/POST ("/admin/" :method :post) (name password)
  (cond
    ((valid-user-p name password)
     (hunchentoot:start-session)
     (setf (hunchentoot:session-value 'name) name)
     (render *template-welcome* :name name))
    (t
     (render *template-login* :name name :error t))))

#++
(hunchentoot:define-easy-handler (admin :uri "/admin/") ()
    (ecase (hunchentoot:request-method*)
      (:get
       (if (loggedin-p)
           (render *template-welcome*)
           (render *template-login*)))
      (:post
       (let ((name (hunchentoot:post-parameter "user"))
             (password (hunchentoot:post-parameter "password")))
         (cond
           ((valid-user-p name password)
            (hunchentoot:start-session)
            (setf (hunchentoot:session-value 'name) name)
            (render *template-welcome* :name name))
           (t
            (render *template-login* :name name :error t)))))
      ))

(hunchentoot:define-easy-handler (admin2 :uri "/admin") ()
  (hunchentoot:redirect "/admin/"))

(hunchentoot:define-easy-handler (logout :uri "/admin/logout") ()
  (setf (hunchentoot:session-value 'name) nil)
  (hunchentoot:redirect "/admin/"))

;; Server.
(defun start-server (&key (port *port*))
  (format t "~&Starting the login demo on port ~a~&" port)
  (unless *server*
    (setf *server* (make-instance 'hunchentoot:easy-acceptor :port port)))
  (hunchentoot:start *server*))

(defun stop-server ()
  (hunchentoot:stop *server*))
