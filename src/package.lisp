;;;; package.lisp

(defpackage #:cl-geos
  (:use #:cl #:cl-geos.bindings)
  (:export #:geometry

           #:geometry-type
           #:geometry-srid
           #:geometry-size

           #:geometry-envelope
           #:geometry-intersection
           #:geometry-difference
           #:geometry-sym-difference
           #:geometry-convex-hull
           #:geometry-boundary
           #:geometry-union
           #:geometry-unary-union
           #:geometry-union-cascaded))
