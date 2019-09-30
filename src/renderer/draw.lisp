(in-package :renderer)

(defun clear (clear-color)
  (check-type clear-color (simple-array single-float (4)))

  (apply #'gl:clear-color (coerce clear-color 'list)) ; Does SBCL optimize this?
  (gl:clear :color-buffer :depth-buffer))

; Per-frame parameters.
(defvar *shader-proj*)
(defvar *shader-view*)

; Per-DRAW-OBJECT-call parameters.
(defvar *shader-model* +identity-xform+)
(defvar *shader-diffuse* (to-float-array '(4) '(1.0 1.0 1.0 1.0)))

(defun draw-object (pos)
  (check-type pos immutable-buffer)

  (check-type *shader-proj* xform)
  (check-type *shader-view* xform)
  (check-type *shader-model* xform)
  (check-type *shader-diffuse* (simple-array single-float (4)))

  (dbg :todo))
