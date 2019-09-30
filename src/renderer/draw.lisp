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

  ; Set things we ought to be able to lift out...
  (gl:use-program (program *renderer*))
  (gl:enable-vertex-attrib-array 0)

  ; Set up vertices.
  (gl:bind-buffer :array-buffer (vbo pos))
  (gl:vertex-attrib-pointer 0 3 :float nil 0 (cffi:null-pointer))

  ; Set up uniforms.
  (gl:uniform-matrix 0 4 (vector (flatten-xform *shader-proj*)))
  (gl:uniform-matrix 1 4 (vector (flatten-xform *shader-view*)))
  (gl:uniform-matrix 2 4 (vector (flatten-xform *shader-model*)))
  (gl:uniformfv 3 *shader-diffuse*)

  ; Do the actual drawing.
  (gl:draw-arrays :triangles 0 (/ (buffer-length pos) 3)))
