
;; (ql:quickload '(:clog :str))

(uiop:define-package :clog-contest
    (:use :cl :clog
          :clog-contest))

(in-package :clog-contest)


;;; Models.

(defclass product ()
  ((id :initarg :id :accessor product-id :type integer
       :documentation "Unique ID")
   (title :initarg :title :accessor product-title :type string)
   (price :initarg :price :accessor product-price :type integer)))

(defvar *product-id* 1
  "Stupid counter to increment our unique product ID.
  Normally this is given by a DB.")

(defparameter *products* '() "A list of products.")

(defun random-price ()
  "Return an integer between 1 and 10.000 (price is expressed in cents)."
  (1+ (random 9999)))

(defparameter *title-part-1* (list "pretty" "little" "awesome" "white" "blue"))

(defparameter *title-part-2* (list "book" "car" "laptop" "travel" "screwdiver"))

(defun random-title ()
  (let ((index (random (length *title-part-1*)))
        (index-2 (random (length *title-part-2*))))
    (format nil "~a ~a" (elt *title-part-1* index) (elt *title-part-2* index-2))))

(defun gen-test-products (&optional (nb 100))
  (dotimes (i nb)
    (push (make-instance 'product
                         :id (incf *product-id*)
                         :title (random-title)
                         :price (random-price))
          *products*))
  *products*)

(defun reset-test-products ()
  (setf *products* nil))

(defun print-product (it &optional (stream nil))
  "Print a product title and price on STREAM (return a new string by default)."
  (format stream "~a - ~f~&"
          (str:fit 20 (product-title it))
          (/ (product-price it) 100)))

(defun print-products (products)
  "Return a list of products as a string (dummy, for tests purposes)."
  (with-output-to-string (s)
    (format s "Products:~&")
    (dolist (it products)
      (print-product it s))))

(defun search-products (query &optional (products *products*))
  "Search for QUERY in the products' title.
  This would be a DB call."
  (loop for product in products
     when (str:containsp (str:downcase query) (str:downcase (product-title product)))
     collect product))


;;; CLOG

(defun start-tutorial ()
  "Start tutorial."
  (initialize 'add-products)
  (open-browser))


(defun add-products (body)
  "Create the search input and a div to contain the products.
  Bind the key-up event of the input field to our filter function."
  (let* ((form (create-form body))
         (input (create-form-element form :input :name "query"
                                     :label
				     (create-label form :content "Filter product: ")))
         (result-div (create-div body :content "" )))

    (set-on-key-up input
                   (lambda (obj event)
                     (format t ":key-up, value: ~a~&" (value obj)) ; logging
                     (setf (text result-div) "") ; this is how we erase the current content.
                     (handle-filter-product result-div obj event)))

    (display-products result-div *products*)))

(defun display-products (parent products)
  "Display these products in the page.
  Create a div per product, with a string to present the product.
  We don't create nice-looking Bulma product cards here."
  (dolist (it products)
      (create-div parent :content
                  (format nil "~a - ~a"
                          (product-id it)
                          (print-product it)))))

(defun handle-filter-product (div obj event)
  "Search and redisplay products."
  ;TODO: wait a little latency
  (declare (ignorable event))
  (let ((query (value obj)))
    (if (> (length query) 2)
        (display-products div (search-products query))
        (print "waiting for more input"))))
