(in-package #:game)

(defvar *projection-matrix* (identity-matrix 4))
(defvar *view-matrix* (identity-matrix 4))

(defclass object ()
  ((name     :initarg :name     :accessor object/name)
   (program  :initarg :program  :accessor object/program)
   (uniforms :initarg :uniforms :accessor object/uniforms)
   (vao      :initarg :vao      :accessor object/vao)))

(defmethod print-object ((obj object) stream)
  (pprint-object-with-slots stream obj '(name program uniforms vao)))

(defmethod draw ((obj object))
  (use-program (object/program obj))
  (bind-uniforms (cons* (cons 0 (make-uniform-float :contents
                                                    (float-vector
                                                      (/ (get-internal-run-time)
                                                         (coerce internal-time-units-per-second 'single-float)))))
                        (cons 1 (make-uniform-mat4 :contents *projection-matrix*))
                        (cons 2 (make-uniform-mat4 :contents *view-matrix*))
                        (object/uniforms obj)))
  (draw-vao (object/vao obj)))
