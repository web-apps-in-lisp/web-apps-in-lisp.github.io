
;;;
;;; Demo of flash messages:
;;; anywhere in a route, easily add flash messages to the session.
;;; They are rendered at the next rendering of a template.
;;; They are removed from the session once rendered.
;;;

;; inspired by https://github.com/rudolfochrist/booker/blob/main/app/controllers.lisp

(uiop:add-package-local-nickname :ht :hunchentoot)

(djula:add-template-directory ".")

(defparameter *flash-template* (djula:compile-template* "flash-template.html"))

(defparameter *port* 9876)

(defvar *server* nil "Our Hunchentoot acceptor")

(defun flash (type message)
  "Add a flash message in the session.

  TYPE: can be anything as you do what you want with it in the template.
     Here, it is a string that represents the Bulma CSS class for notifications: is-primary, is-warning etc.
  MESSAGE: string"
  (let* ((session (ht:start-session))
         (flash (ht:session-value :flash session)))
    (setf (ht:session-value :flash session)
          ;; With a cons, REST returns 1 element
          ;; (when with a list, REST returns a list)
          (cons (cons type message) flash))))

;;; delete flash after it is used.
(defmethod ht:handle-request :after (acceptor request)
  (ht:delete-session-value :flash))

#+another-solution
(defun render (template &rest args)
  (apply
   #'djula:render-template* template nil
   (list*
    :flashes (or (ht:session-value :flash)
               (list (cons "is-primary" "No more flash messages were found in the session. This is a default notification.")))
    args)))


(easy-routes:defroute flash-route ("/flash/" :method :get) ()
  #-another-solution
  (djula:render-template*  *flash-template* nil
                           :flashes (or (ht:session-value :flash)
                                        (list (cons "is-primary" "No more flash messages were found in the session. This is a default notification."))))
  #+another-solution
  (render *flash-template*)
  )

(easy-routes:defroute flash-redirect-route ("/tryflash/") ()
  (flash "is-warning" "This is a warning message held in the session. It should appear only once: reload this page and you won't see the flash message again.")
  (ht:redirect "/flash/"))

(defun start (&key (port *port*))
  (format t "~&Starting the web server on port ~a~&" port)
  (force-output)
  (setf *server* (make-instance 'easy-routes:easy-routes-acceptor :port port))
  (ht:start *server*))

(defun stop ()
  (ht:stop *server*))
