(load "myproject.asd")

(ql:quickload "myproject")

(sb-ext:save-lisp-and-die "myproject"
                          :executable t
                          :toplevel #'myproject::main
                          :compression 9)
