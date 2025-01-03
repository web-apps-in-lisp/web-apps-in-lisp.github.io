(asdf:defsystem "myproject"
  :version "0.1"
  :author "me"
  :license "WTFPL"
  :depends-on (
               :hunchentoot  ;; web server
               :easy-routes  ;; routes facility
               :djula        ;; HTML templates

               ;; utils
               :str          ;; strings library
               :find-port
               )
  :components ((:module "src"  ;; a src/ subdirectory
                :components
                (
                 (:file "packages")  ;; = src/packages.lisp
                 (:file "myproject") ;; = src/myproject.lisp
                )))

  ;; To build a binary:
  :build-operation "program-op"
  :build-pathname "myproject"
  :entry-point "myproject::main"

  :description "A list of products")
