;;;; package.lisp

(defpackage #:cl-geos
  (:use #:cl #:cl-geos.bindings)
  (:export #:geometry
           #:geometry-type
           #:geometry-srid
           #:geometry-size))
