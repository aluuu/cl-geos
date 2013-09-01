;;;; cl-geos.lisp

(in-package #:cl-geos)

(defclass geometry ()
  ((pointer :initarg :pointer :accessor geometry-pointer :type %Geometry)
   (srid :initarg :srid :type integer)))

(defmethod initialize-instance :around ((object geometry) &key (wkt "POINT(0 0)") (srid nil))
  (with-geos
    (setf (geometry-pointer object) (geos-geometry-from-wkt wkt))
    (when srid
      (setf (geos-geometry-srid object) srid))))

(defmethod geometry-srid ((object geometry))
  (with-geos
    (geos-geometry-get-srid (geometry-pointer object))))

(defmethod (setf geometry-srid) ((new-value integer) (object geometry))
  (with-geos
    (geos-geometry-set-srid (geometry-pointer object) new-value)
    new-value))

(defmethod geometry-type ((object geometry))
  (geos-geometry-type-id (geometry-pointer object)))
