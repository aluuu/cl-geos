;;;; cl-geos.asd

(asdf:defsystem #:cl-geos
  :serial t
  :description "Describe cl-geos here"
  :author "Alexander Dinu <aluuu@husa.su>"
  :license "Specify license here"
  :depends-on (#:cl-geos.bindings)
  :components ((:file "package")
               (:file "cl-geos")))

(asdf:defsystem #:cl-geos.bindings
  :serial t
  :description "Describe cl-geos here"
  :author "Alexander Dinu <aluuu@husa.su>"
  :license "Specify license here"
  :depends-on (#:cffi #:cl-utilities)
  :components ((:file "bindings")))
