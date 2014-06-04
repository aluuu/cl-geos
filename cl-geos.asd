;;;; cl-geos.asd

(asdf:defsystem #:cl-geos
  :serial t
  :description "Describe cl-geos here"
  :author "Alexander Dinu <aluuu@husa.su>"
  :license "Specify license here"
  :depends-on (#:cffi #:cl-utilities #:fiveam #:rutils)
  :components ((:module :cl-geos.bindings
                :pathname "src/bindings/"
                :serial t
                :components ((:file "package")
                             (:file "bindings")))

               (:module :cl-geos
                        :pathname "src/"
                        :components ((:file "package")
                                     (:file "geos"))
                        :depends-on (:cl-geos.bindings))

               (:module :cl-geos.tests
                        :pathname "tests/"
                        :serial t
                        :components ((:file "package")
                                     (:file "tests"))
                        :depends-on (:cl-geos))))
