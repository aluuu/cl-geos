(defpackage #:cl-geos.bindings
  (:use #:cl #:cffi #:cl-utilities)
  (:export #:with-geos
           #:geos-error
           #:geometry-from-wkt
           #:geometry-to-wkt
           #:geometry-destroy
           #:geometry-type-id
           #:geometry-set-srid
           #:geometry-get-srid
           #:geometry-get-num-geometries
           #:geometry-get-geometry-n
           #:geometry-get-num-points

           #:geometry-envelope

           #:coordseq-create
           #:coordseq-destroy
           #:coordseq-setx
           #:coordseq-getx
           #:coordseq-sety
           #:coordseq-gety
           #:coordseq-setz
           #:coordseq-getz
           #:coordseq-set-ordinate
           #:coordseq-get-ordinate))
