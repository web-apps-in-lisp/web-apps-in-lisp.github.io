(asdf:defsystem "myproject"
  :version "0.1"
  :author "me"
  :license "WTFPL"
  :depends-on (
               :hunchentoot  ;; web server
               :easy-routes  ;; routes facility
               :djula        ;; HTML templates

               ;; utils
               :find-port
               )
  :components ((:module "src"  ;; a src/ subdirectory
                :components
                ((:file "myproject") ;; = src/myproject.lisp
                )))

  :description "A list of products")
