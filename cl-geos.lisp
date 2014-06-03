;;;; cl-geos.lisp

(in-package #:cl-geos)

(defclass geometry ()
  ((pointer :initarg :pointer :accessor geometry-pointer :type %Geometry)
   (srid :initarg :srid :type integer)))

(defmethod initialize-instance :around ((object geometry) &key (wkt "POINT(0 0)") (srid nil))
  (with-geos
    (setf (geometry-pointer object) (cl-geos.bindings:geometry-from-wkt wkt))
    (when srid
      (setf (geometry-srid object) srid))))

(defmethod geometry-srid ((object geometry))
  (with-geos
    (cl-geos.bindings:geometry-get-srid (geometry-pointer object))))

(defmethod (setf geometry-srid) ((new-value integer) (object geometry))
  (with-geos
    (cl-geos.bindings:geometry-set-srid (geometry-pointer object) new-value)
    new-value))

(defmethod geometry-type ((object geometry))
  (with-geos
    (cl-geos.bindings:geometry-type-id (geometry-pointer object))))

(defmethod geometry-size ((object geometry))
  (with-geos
    (if (equal :linestring (geometry-type object))
        (cl-geos.bindings:geometry-get-num-points (geometry-pointer object))
        (cl-geos.bindings:geometry-get-num-geometries (geometry-pointer object)))))

(defmethod geometry-envelope ((object geometry))
  (with-geos
    (cl-geos.bindings:geometry-envelope (geometry-pointer object))))
