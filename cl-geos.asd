;;;; cl-geos.asd
(asdf:defsystem #:cl-geos
  :serial t
  :description "Describe cl-geos here"
  :author "Alexander Dinu <aluuu@husa.su>"
  :license "Specify license here"
  :depends-on (#:cffi #:cl-utilities #:fiveam)
  :components ((:module "bindings"
                :pathname "bindings/"
                :serial t
                :components ((:file "package")
                             (:file "bindings")))

               (:file "package")
               (:file "cl-geos")))
