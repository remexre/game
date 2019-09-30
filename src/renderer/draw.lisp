(in-package :renderer)

(defun clear (clear-color)
  (check-type clear-color (simple-array single-float (4)))

  ; Does SBCL optimize this?
  (apply #'gl:clear-color (coerce clear-color 'list))

  (gl:clear :color-buffer :depth-buffer))

(defun draw-object (&key pos proj view model)
  (check-type pos immutable-buffer)
  (check-type proj xform)
  (check-type view xform)
  (check-type model xform)

  (dbg :todo))
