(defpackage myproject
  (:use :cl))

(in-package :myproject)

(defvar *server* nil
  "Server instance (Hunchentoot acceptor).")

(defparameter *port* 8899 "The application port.")

(defparameter *template-root* "
<title> Lisp web app </title>
<head>
  <link
  rel=\"stylesheet\"
  href=\"https://cdn.jsdelivr.net/npm/@picocss/pico@2/css/pico.min.css\">
</head>
<body>
  <ul>
  {% for product in products %}
    <li>
      <a href=\"/product/{{ product.0 }}\">{{ product.1 }} - {{ product.2 }}</a>
    </li>
  {% endfor %}
 </ul>
</body>
")

(defparameter *template-root* "

<html>
<head>
  <link
  rel=\"stylesheet\"
  href=\"https://cdn.jsdelivr.net/npm/@picocss/pico@2/css/pico.min.css\">
</head>
<body class=\"container\">
 <form action=\"/\" method=\"GET\">
  <div>
    <label for=\"query\">What do you search for?</label>
    <input name=\"query\" id=\"query\" placeholder=\"Search…\" />
  </div>
  <div>
    <button>Search</button>
  </div>
</form>

{% if query %}
<div> query is: {{ query }} </div>

<ul>
  {% for product in results %}
    <li>
      <a href=\"/product/{{ product.0 }}\">{{ product.1 }} - {{ product.2 }}</a>
    </li>
  {% endfor %}
</ul>
{% endif %}
</body>
</html>
")

(defparameter *template-product* "
<body>
     name: {{ product.1 }} price: {{ product.2 }}

{% if debug %} debug info! {% endif %}
</body>
")

(defun products (&optional (n 5))
  (loop for i from 0 below n
        collect (get-product i)))

(defun get-product (n)
  (list n (format nil "Product nb ~a" n) 9.99))

(defun search-products (products query)
  (loop for product in products
        if (search query (second product) :test #'equalp)
          collect product))

(defun render (template &rest args)
  (apply
   #'djula:render-template*
   (djula:compile-string template)
   nil
   args))

(easy-routes:defroute root ("/") (query)
  (render *template-root*
          :results (search-products (products) query)
          :query query))

(easy-routes:defroute product-route ("/product/:n") (&get debug &path (n 'integer))
  (render *template-product*
          :product (get-product n)
          :debug debug))


(defun start-server (&key (port *port*))
  (format t "~&Starting the web server on port ~a~&" port)
  (force-output)
  (setf *server* (make-instance 'easy-routes:easy-routes-acceptor
                                :port port))
  (hunchentoot:start *server*))

;;; Top-level.
(defun main ()
  (start-server :port (find-port:find-port :min *port*))
  (sleep most-positive-fixnum))
