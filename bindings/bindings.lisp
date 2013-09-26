;;;; bindings.lisp
(in-package #:cl-geos.bindings)

(defparameter *geos-context-handle* nil)

(define-condition geos-error (error)
  ((message :initarg :message :reader geos-error-message))
  (:report
   (lambda (condition stream)
     (format stream "~A" (geos-error-message condition)))))

(defun geos-error (message)
  (error 'geos-error :message message))

(defmacro with-geos (&body body)
  `(let* ((*geos-context-handle* (geos-init))
          (result (progn ,@body)))
     (geos-finish *geos-context-handle*)
     result))

(defmacro defgeos (name args &body body)
  `(defun ,name ,args
     (if (null *geos-context-handle*)
         (error (format nil "You must call #'~a function inside WITH-GEOS macro or manually call GEOS-INIT at the beginning of your program" ,(symbol-name name)))
         (progn ,@body))))

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

(defun geos-finish (*geos-context-handle*)
  (when *geos-context-handle*
    (finishGEOS-r *geos-context-handle*))
  (setf *geos-context-handle* nil))

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

(defcfun "GEOSGeomType_r" :pointer
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

(defgeos geometry-from-wkt (string)
  "CHECK VERSION! DEPRICATED"
  (with-foreign-string (foreign-string string)
    (GEOSGeomFromWKT-r *geos-context-handle* foreign-string)))

(defgeos geometry-to-wkt (geometry)
  "CHECK VERSION! DEPRICATED"
  (foreign-string-to-lisp (GEOSGeomToWKT-r *geos-context-handle* geometry)))

(defgeos geometry-destroy (geometry)
  (GEOSGeom-destroy-r *geos-context-handle* geometry))

;; (defgeos geometry-type (geometry)
;;   (foreign-string-to-lisp (GEOSGeomType-r *geos-context-handle* geometry)))

(defgeos geometry-type-id (geometry)
  (GEOSGeomTypeId-r *geos-context-handle* geometry))

(defgeos geometry-get-srid (geometry)
  (GEOSGetSRID-r *geos-context-handle* geometry))

(defgeos geometry-set-srid (geometry srid)
  (GEOSSetSRID-r *geos-context-handle* geometry srid))

(defgeos geometry-get-num-geometries (geometry)
  (GEOSGetNumGeometries-r *geos-context-handle* geometry))

(defgeos geometry-get-num-points (geometry)
  (GEOSGeomGetNumPoints-r *geos-context-handle* geometry))

(defgeos geometry-get-geometry-n (geometry n)
  "CHECK VERSION!
   Up to GEOS 3.2.0 the input geometry must be a Collection, in
   later version it doesn't matter (getGeometryN(0) for a single will return the input)."
  (GEOSGetGeometryN-r *geos-context-handle* geometry n))

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

(defgeos coordseq-create (size dims)
  (GEOSCoordSeq-create-r *geos-context-handle* size dims))

(defgeos coordseq-destroy (coordseq)
  (GEOSCoordSeq-destroy-r *geos-context-handle* coordseq))

(defgeos coordseq-setx (coordseq idx value)
  (GEOSCoordSeq-SetX-r *geos-context-handle* coordseq idx (coerce value 'double-float)))

(defgeos coordseq-getx (coordseq idx)
  (with-foreign-object (value :double)
    (GEOSCoordSeq-getX-r *geos-context-handle* coordseq idx value)
    (mem-ref value :double)))

(defgeos coordseq-sety (coordseq idx value)
  (GEOSCoordSeq-setY-r *geos-context-handle* coordseq idx (coerce value 'double-float)))

(defgeos coordseq-gety (coordseq idx)
  (with-foreign-object (value :double)
    (GEOSCoordSeq-getY-r *geos-context-handle* coordseq idx value)
    (mem-ref value :double)))

(defgeos coordseq-setz (coordseq idx value)
  (GEOSCoordSeq-setZ-r *geos-context-handle* coordseq idx (coerce value 'double-float)))

(defgeos coordseq-getz (coordseq idx)
  (with-foreign-object (value :double)
    (GEOSCoordSeq-getZ-r *geos-context-handle* coordseq idx value)
    (mem-ref value :double)))

(defgeos coordseq-set-ordinate (coordseq idx dim value)
  (GEOSCoordSeq-setOrdinate-r *geos-context-handle* coordseq idx dim (coerce value 'double-float)))

(defgeos coordseq-get-ordinate (coordseq idx dim)
  (with-foreign-object (value :double)
    (GEOSCoordSeq-getOrdinate-r *geos-context-handle* coordseq idx dim value)
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
