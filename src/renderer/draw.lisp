(in-package :renderer)

; Per-frame parameters.
(defvar *shader-proj-xform*)
(defvar *shader-view-xform*)
(defvar *shader-light-position* (to-float-array '(3) '(0.0 0.0 0.0)))

; Per-DRAW-OBJECT-call parameters.
(defvar *shader-model-xform* +identity-xform+)
(defvar *shader-diffuse* (to-float-array '(3) '(1.0 0.0 1.0)))
(defvar *shader-ambient* (to-float-array '(3) '(0.1 0.1 0.1)))

; For logging purposes.
(defvar *drawn-triangles* 0)

(defun clear (clear-color)
  (check-type clear-color (simple-array single-float (4)))

  (apply #'gl:clear-color (coerce clear-color 'list)) ; Does SBCL optimize this?
  (gl:clear :color-buffer :depth-buffer)
  
  (setf *drawn-triangles* 0))

(defun draw-object (pos &key cull-radius)
  (check-type pos immutable-buffer)

  (check-type *shader-proj-xform* xform)
  (check-type *shader-view-xform* xform)
  (check-type *shader-model-xform* xform)
  (check-type *shader-ambient* (simple-array single-float (3)))
  (check-type *shader-diffuse* (simple-array single-float (3)))

  ; Try to cull.
  (when cull-radius
    (check-type cull-radius single-float)
    (let* ((model-to-camera (compose-xforms *shader-view-xform* *shader-model-xform*))
           (center (apply-xform-unit-w model-to-camera))
           (z (aref center 2)))
      (when (> z cull-radius)
        (return-from draw-object))))

  ; Set up vertices.
  (gl:bind-buffer :array-buffer (vbo pos))
  (gl:vertex-attrib-pointer 0 3 :float nil 32 0)
  (gl:vertex-attrib-pointer 1 2 :float nil 32 12)
  (gl:vertex-attrib-pointer 2 3 :float nil 32 20)

  ; Set up uniforms.
  (gl:uniform-matrix 0 4 (vector (flatten-xform *shader-proj-xform*)))
  (gl:uniform-matrix 1 4 (vector (flatten-xform *shader-view-xform*)))
  (gl:uniform-matrix 2 4 (vector (flatten-xform *shader-model-xform*)))
  (gl:uniformfv 3 *shader-diffuse*)
  (gl:uniformfv 4 *shader-ambient*)
  (gl:uniformfv 5 *shader-light-position*)

  ; Do the actual drawing.
  (let ((len (/ (buffer-length pos) 8)))
    (incf *drawn-triangles* len)
    (gl:draw-arrays :triangles 0 len)))
