;;;; cl-geos.lisp

(in-package #:cl-geos)

(defclass geometry ()
  ((ctx :initarg :ctx :accessor ctx :type %ContextHandle)
   (pointer :initarg :pointer :accessor geometry-pointer :type %Geometry)
   (srid :initarg :srid :accessor geometry-srid :type integer)))

(defmethod initialize-instance :around ((object geometry) &key (wkt "POINT(0 0)") (srid nil) (pointer nil))
  (setf (ctx object)
        (cl-geos.bindings::geos-init))
  (if pointer
      (setf (geometry-pointer object) pointer)
      (if wkt
          (setf (geometry-pointer object)
                (cl-geos.bindings::geometry-from-wkt (ctx object) wkt))))
  (when srid
    (setf (geometry-srid object) srid)))

(defmethod geometry-wkt ((object geometry))
  (cl-geos.bindings::geometry-to-wkt
   (ctx object) (geometry-pointer object)))

(defmethod geometry-srid ((object geometry))
  (cl-geos.bindings::geometry-get-srid
   (ctx object) (geometry-pointer object)))

(defmethod (setf geometry-srid) ((new-value integer) (object geometry))
  (cl-geos.bindings::geometry-set-srid
   (ctx object) (geometry-pointer object) new-value)
  new-value)

(defmethod geometry-type ((object geometry))
  (cl-geos.bindings::geometry-type-id (ctx object) (geometry-pointer object)))

(defmethod geometry-size ((object geometry))
  (if (equal :linestring (geometry-type object))
      (cl-geos.bindings::geometry-get-num-points
       (ctx object) (geometry-pointer object))
      (cl-geos.bindings::geometry-get-num-geometries
       (ctx object) (geometry-pointer object))))

(defmethod geometry-envelope ((object geometry))
  (cl-geos.bindings::geometry-envelope
   (ctx object) (geometry-pointer object)))

(defmethod geometry-intersection ((object1 geometry) (object2 geometry))
  (make-instance 'geometry
                 :pointer (cl-geos.bindings::geometry-intersection
                           (ctx object1) (geometry-pointer object1) (geometry-pointer object2))))

(defmethod geometry-convex-hull ((object geometry))
  (cl-geos.bindings::geometry-convex-hull
   (ctx object) (geometry-pointer object)))

(defmethod geometry-difference ((object1 geometry) (object2 geometry))
  (cl-geos.bindings::geometry-difference
   (ctx object1) (geometry-pointer object1) (geometry-pointer object2)))

(defmethod geometry-sym-difference ((object1 geometry) (object2 geometry))
  (cl-geos.bindings::geometry-sym-difference
   (ctx object1) (geometry-pointer object1) (geometry-pointer object2)))

(defmethod geometry-boundary ((object geometry))
  (cl-geos.bindings::geometry-boundary
   (ctx object) (geometry-pointer object)))

(defmethod geometry-union ((object1 geometry) (object2 geometry))
  (cl-geos.bindings::geometry-union
   (ctx object1) (geometry-pointer object1) (geometry-pointer object2)))

(defmethod geometry-unary-union ((object geometry))
  (cl-geos.bindings::geometry-unary-union
   (ctx object) (geometry-pointer object)))

(defmethod geometry-union-cascaded ((object geometry))
  (cl-geos.bindings::geometry-union-cascaded
   (ctx object) (geometry-pointer object)))
