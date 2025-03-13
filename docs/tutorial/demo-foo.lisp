
(defpackage :foo-for-ari
  (:use :cl))

(in-package :foo-for-ari)

(defun hello ()
  ;; C-c C-y
  :hello)

;; (defconstant +pi+ 3.14)  ;; too cumbersome
;; (defparameter +pi+ 3.14)

;; defvar: not modified again when using C-c C-c or (most importantly) C-c C-k or load
(defvar *server* nil
  "Internal variable. Change with SETF.")

(defvar *db-connection* nil)

(with-db-connection (conn)
  (let ((*db-connection* conn))
    …))

;; parameters are modified at each C-c C-c or C-c C-k or load
(defparameter *param* 2)


(let (foo bar)
  (setf foo 1)
  (print foo))

;; a LET scope
(let* ((foo 1)
       (bar (1+ foo)))
  (setf foo 3)
  (print foo)
  (print bar))
;; out of the LET scope
(print foo)

;; bind global ("special") variables/parameters
(let ((*param* 333))
  (function-using-param)
  (format t "*param* is: ~a" *param*))

(flet ())  ;; = let

(labels ()) ;; = let* for functions

;; globals are thread-local: don't worry.
;; (see: Hunchentoot uses *globals* for the current request, etc)
