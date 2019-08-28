(in-package #:game)

(defvar *projection-matrix* (identity-matrix 4))
(defvar *view-matrix* (identity-matrix 4))

(defclass object ()
  ((name     :initarg :name     :reader object/name)
   (program  :initarg :program  :reader object/program)
   (uniforms :initarg :uniforms :reader object/uniforms)
   (vao      :initarg :vao      :reader object/vao)))

(defmethod print-object ((obj object) stream)
  (pprint-object-with-slots stream obj '(name program vao)))

(defmethod draw ((obj object))
  (use-program (object/program obj))
  (bind-uniforms (append-to (cons 0 (make-uniform-mat4 :contents *projection-matrix*))
                            (cons 1 (make-uniform-mat4 :contents *view-matrix*))
                            (object/uniforms obj)))
  (draw-vao (object/vao obj)))
