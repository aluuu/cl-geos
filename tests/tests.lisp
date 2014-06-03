(in-package #:cl-geos.tests)

(def-suite geometry-suite :description "Testing basic geometry operations")

(in-suite geometry-suite)

(defun gen-coordinates (num)
  (loop for i from 1 to num
        for j from 1 to num
        collect (list (- (random 100.0) 50) (- (random 100.0) 50))))

(defun gen-wkt (geometry-type &key (length 2))
  (case geometry-type
    (:point
     (format nil "POINT(~{~{~a ~a~}~})"
             (gen-coordinates 1)))
    (:linestring
     (format nil "LINESTRING(~{~{~a ~a~}~^,~})"
             (gen-coordinates length)))
    (:polygon
     (let* ((points (gen-coordinates length))
            (points (rutils:zip points (append (rest points) `(,(first points)))))
            (points (append points `(,(first points))))
            (points (map 'list #'first points)))
       (format nil "POLYGON((~{~{~a ~a~}~^,~}))" points)))))

(test point-creation
  (for-all ((simple-point #'(lambda () (make-instance 'cl-geos:geometry :wkt (gen-wkt :point)))))
    (is (eq (geometry-type simple-point) :point))
    (is (= (geometry-srid simple-point) 0))
    (is (= (geometry-size simple-point) 1))))

(test linestring-creation
  (for-all ((simple-line #'(lambda () (make-instance 'cl-geos:geometry :wkt (gen-wkt :linestring :length 5)))))
    (is (eq (geometry-type simple-line) :linestring))
    (is (= (geometry-srid simple-line) 0))
    (is (= (geometry-size simple-line) 5))))

(test polygon-creation
  (for-all ((simple-polygon #'(lambda () (make-instance 'cl-geos:geometry :wkt (gen-wkt :polygon :length 3)))))
    (is (eq (geometry-type simple-polygon) :polygon))
    (is (= (geometry-srid simple-polygon) 0))
    (is (= (geometry-size simple-polygon) 1))))

(run! 'geometry-suite)
