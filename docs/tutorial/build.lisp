(load "myproject.asd")

(ql:quickload "myproject")

(setf uiop:*image-entry-point* #'myproject::main)

(uiop:dump-image "myproject"
                          :executable t
                          ;; :toplevel #'myproject::main
                          ;; :entry-point  #'myproject::main
                          :compression 9)
