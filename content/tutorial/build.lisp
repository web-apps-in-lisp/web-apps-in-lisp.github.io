(load "myproject.asd")

(ql:quickload "myproject")

(setf uiop:*image-entry-point* #'myproject::main)

(uiop:dump-image "myproject" :executable t :compression 9)

#|
;; The same as:

(sb-ext:save-lisp-and-die "myproject" :executable t :compression 9
                          :toplevel #'myproject::main)

|#
