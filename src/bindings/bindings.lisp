;;;; bindings.lisp
(in-package #:cl-geos.bindings)

(define-condition geos-error (error)
  ((message :initarg :message :reader geos-error-message))
  (:report
   (lambda (condition stream)
     (format stream "~A" (geos-error-message condition)))))

(defun geos-error (message)
  (error 'geos-error :message message))

(define-foreign-library libgeos
  (:unix (:or "libgeos_c.so.1" "libgeos_c.so"))
  (t (:default "libgeos_c")))

(use-foreign-library libgeos)

(defcfun "GEOSversion" :pointer)

(defun geos-version ()
  (let* ((version-string (with-foreign-pointer-as-string (str 0 :encoding :ascii)
                          (setf str (GEOSVersion))
                          (foreign-string-to-lisp str)))
        (geos-version-string (first (split-sequence #\- version-string))))
    (map 'list #'read-from-string (split-sequence #\. geos-version-string))))

(defctype %ContextHandle :pointer)

(defcallback geos-notice-handler :void ((fmt :pointer) (list :pointer))
  (declare (ignore fmt))
  (format *standard-output* "GEOS notice: ~A" (foreign-string-to-lisp list)))

(defcallback geos-error-handler :void ((fmt :pointer) (list :pointer))
  (declare (ignore fmt))
  (geos-error (foreign-string-to-lisp list)))

(defcfun "initGEOS_r" %ContextHandle
  (notice-func :pointer)
  (error-func :pointer))

(defcfun "finishGEOS_r" :void
  (handle %ContextHandle))

(defun geos-init ()
  (initGEOS-r (get-callback 'geos-notice-handler) (get-callback 'geos-error-handler)))

(defun geos-finish (ctx)
  (when ctx
    (finishGEOS-r ctx)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GEOSGeom_* bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defctype %Geometry :pointer)

(defcenum %GeomTypes
  :point
  :linestring
  :linearring
  :polygon
  :multipoint
  :multilinestring
  :multipolygon
  :geometrycollection)

(defcfun "GEOSGeomFromWKT_r" %Geometry
  (handle %ContextHandle)
  (wkt :pointer))

(defcfun "GEOSGeomToWKT_r" :pointer
  (handle %ContextHandle)
  (geometry %Geometry))

(defcfun "GEOSGeom_destroy_r" :void
  (handle %ContextHandle)
  (geometry %Geometry))

(defcfun "GEOSGeomTypeId_r" %GeomTypes
  (handle %ContextHandle)
  (geometry %Geometry))

(defcfun "GEOSGetSRID_r" :int
  (handle %ContextHandle)
  (geometry %Geometry))

(defcfun "GEOSSetSRID_r" :void
  (handle %ContextHandle)
  (geometry %Geometry)
  (srid :int))

(defcfun "GEOSGetNumGeometries_r" :int
  (handle %ContextHandle)
  (geometry %Geometry))

(defcfun "GEOSGetGeometryN_r" %Geometry
  (handle %ContextHandle)
  (geometry %Geometry)
  (n :int))

(defcfun "GEOSGeomGetNumPoints_r" :int
  (handle %ContextHandle)
  (geometry %Geometry))

(defun geometry-from-wkt (ctx string)
  "CHECK VERSION! DEPRICATED"
  (with-foreign-string (foreign-string string)
    (GEOSGeomFromWKT-r ctx foreign-string)))

(defun geometry-to-wkt (ctx geometry)
  "CHECK VERSION! DEPRICATED"
  (foreign-string-to-lisp (GEOSGeomToWKT-r ctx geometry)))

(defun geometry-destroy (ctx geometry)
  (GEOSGeom-destroy-r ctx geometry))

(defun geometry-type-id (ctx geometry)
  (GEOSGeomTypeId-r ctx geometry))

(defun geometry-get-srid (ctx geometry)
  (GEOSGetSRID-r ctx geometry))

(defun geometry-set-srid (ctx geometry srid)
  (GEOSSetSRID-r ctx geometry srid))

(defun geometry-get-num-geometries (ctx geometry)
  (GEOSGetNumGeometries-r ctx geometry))

(defun geometry-get-num-points (ctx geometry)
  (GEOSGeomGetNumPoints-r ctx geometry))

(defun geometry-get-geometry-n (ctx geometry n)
  "CHECK VERSION!
   Up to GEOS 3.2.0 the input geometry must be a Collection, in
   later version it doesn't matter (getGeometryN(0) for a single will return the input)."
  (GEOSGetGeometryN-r ctx geometry n))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Topology operations
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun "GEOSEnvelope_r" %Geometry
  (handle %ContextHandle)
  (geometry %Geometry))

(defcfun "GEOSIntersection_r" %Geometry
  (handle %ContextHandle)
  (geometry1 %Geometry)
  (geometry2 %Geometry))

(defcfun "GEOSConvexHull_r" %Geometry
  (handle %ContextHandle)
  (geometry %Geometry))

(defcfun "GEOSDifference_r" %Geometry
  (handle %ContextHandle)
  (geometry1 %Geometry)
  (geometry2 %Geometry))

(defcfun "GEOSSymDifference_r" %Geometry
  (handle %ContextHandle)
  (geometry1 %Geometry)
  (geometry2 %Geometry))

(defcfun "GEOSBoundary_r" %Geometry
  (handle %ContextHandle)
  (geometry %Geometry))

(defcfun "GEOSUnion_r" %Geometry
  (handle %ContextHandle)
  (geometry1 %Geometry)
  (geometry2 %Geometry))

(defcfun "GEOSUnaryUnion_r" %Geometry
  (handle %ContextHandle)
  (geometry %Geometry))

(defcfun "GEOSUnionCascaded_r" %Geometry
  (handle %ContextHandle)
  (geometry %Geometry))

(defcfun "GEOSPointOnSurface_r" %Geometry
  (handle %ContextHandle)
  (geometry %Geometry))

(defcfun "GEOSGetCentroid_r" %Geometry
  (handle %ContextHandle)
  (geometry %Geometry))

(defcfun "GEOSNode_r" %Geometry
  (handle %ContextHandle)
  (geometry %Geometry))

(defun geometry-envelope (ctx geometry)
  (GEOSEnvelope-r ctx geometry))

(defun geometry-intersection (ctx geometry1 geometry2)
  (GEOSIntersection-r ctx geometry1 geometry2))

(defun geometry-convex-hull (ctx geometry)
  (GEOSConvexHull-r ctx geometry))

(defun geometry-difference (ctx geometry1 geometry2)
  (GEOSDifference-r ctx geometry1 geometry2))

(defun geometry-sym-difference (ctx geometry1 geometry2)
  (GEOSSymDifference-r ctx geometry1 geometry2))

(defun geometry-boundary (ctx geometry)
  (GEOSBoundary-r ctx geometry))

(defun geometry-union (ctx geometry1 geometry2)
  (GEOSUnion-r ctx geometry1 geometry2))

(defun geometry-unary-union (ctx geometry)
  (GEOSUnaryUnion-r ctx geometry))

(defun geometry-union-cascaded (ctx geometry)
  (GEOSUnionCascaded-r ctx geometry))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GEOSCoordSeq_* bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defctype %CoordSeq :pointer)

(defcfun "GEOSCoordSeq_create_r" %CoordSeq
  (handle %ContextHandle)
  (size :unsigned-int)
  (dims :unsigned-int))

(defcfun "GEOSCoordSeq_destroy_r" :void
  (handle %ContextHandle)
  (coord-seq %CoordSeq))

(defcfun "GEOSCoordSeq_setX_r" :int
  (handle %ContextHandle)
  (coord-seq %CoordSeq)
  (idx :unsigned-int)
  (value :double))

(defcfun "GEOSCoordSeq_getX_r" :int
  (handle %ContextHandle)
  (coord-seq %CoordSeq)
  (idx :unsigned-int)
  (value :pointer))

(defcfun "GEOSCoordSeq_setY_r" :int
  (handle %ContextHandle)
  (coord-seq %CoordSeq)
  (idx :unsigned-int)
  (value :double))

(defcfun "GEOSCoordSeq_getY_r" :int
  (handle %ContextHandle)
  (coord-seq %CoordSeq)
  (idx :unsigned-int)
  (value :pointer))

(defcfun "GEOSCoordSeq_setZ_r" :int
  (handle %ContextHandle)
  (coord-seq %CoordSeq)
  (idx :unsigned-int)
  (value :double))

(defcfun "GEOSCoordSeq_getZ_r" :int
  (handle %ContextHandle)
  (coord-seq %CoordSeq)
  (idx :unsigned-int)
  (value :pointer))

(defcfun "GEOSCoordSeq_setOrdinate_r" :int
  (handle %ContextHandle)
  (coord-seq %CoordSeq)
  (idx :unsigned-int)
  (dim :unsigned-int)
  (value :double))

(defcfun "GEOSCoordSeq_getOrdinate_r" :int
  (handle %ContextHandle)
  (coord-seq %CoordSeq)
  (idx :unsigned-int)
  (dim :unsigned-int)
  (value :pointer))

(defcfun "GEOSCoordSeq_getSize_r" :int
  (handle %ContextHandle)
  (coord-seq %CoordSeq)
  (size :double))

(defcfun "GEOSCoordSeq_getDimensions_r" :int
  (handle %ContextHandle)
  (coord-seq %CoordSeq)
  (dims :pointer))

(defcfun "GEOSGeom_getCoordSeq_r" %CoordSeq
  (handle %ContextHandle)
  (geometry %Geometry))

(defun coordseq-create (ctx size dims)
  (GEOSCoordSeq-create-r ctx size dims))

(defun coordseq-destroy (ctx coordseq)
  (GEOSCoordSeq-destroy-r ctx coordseq))

(defun coordseq-setx (ctx coordseq idx value)
  (GEOSCoordSeq-SetX-r ctx coordseq idx (coerce value 'double-float)))

(defun coordseq-getx (ctx coordseq idx)
  (with-foreign-object (value :double)
    (GEOSCoordSeq-getX-r ctx coordseq idx value)
    (mem-ref value :double)))

(defun coordseq-sety (ctx coordseq idx value)
  (GEOSCoordSeq-setY-r ctx coordseq idx (coerce value 'double-float)))

(defun coordseq-gety (ctx coordseq idx)
  (with-foreign-object (value :double)
    (GEOSCoordSeq-getY-r ctx coordseq idx value)
    (mem-ref value :double)))

(defun coordseq-setz (ctx coordseq idx value)
  (GEOSCoordSeq-setZ-r ctx coordseq idx (coerce value 'double-float)))

(defun coordseq-getz (ctx coordseq idx)
  (with-foreign-object (value :double)
    (GEOSCoordSeq-getZ-r ctx coordseq idx value)
    (mem-ref value :double)))

(defun coordseq-set-ordinate (ctx coordseq idx dim value)
  (GEOSCoordSeq-setOrdinate-r ctx coordseq idx dim (coerce value 'double-float)))

(defun coordseq-get-ordinate (ctx coordseq idx dim)
  (with-foreign-object (value :double)
    (GEOSCoordSeq-getOrdinate-r ctx coordseq idx dim value)
    (mem-ref value :double)))

(defctype %WKTReader :pointer)

(defctype %WKTWriter :pointer)

(defctype %WKBReader :pointer)

(defctype %WKBWriter :pointer)

(defcenum %ByteOrders
  (:WKB_XDR 0)
  (:WKB_NDR 1))

(defcenum %BufCapStyles
  (:cap-round 1)
  (:cap-flat 2)
  (:cat-square 3))

(defcenum %BufJoinStyles
  (:join-round 1)
  (:join-mitre 2)
  (:join-bevel 3))
