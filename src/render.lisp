(in-package #:game)

(defvar *render-objects*)

(defun add-render-object (obj)
  (check-type obj object)
  (vector-push-extend obj *render-objects*))

(defun add-render-objects (objs)
  (iter
    (for obj in-sequence objs)
    (add-render-object obj)))

(defun clear-render-objects ()
  (setf *render-objects* (make-array '(128) :element-type 'object :adjustable t :fill-pointer 0)))

(clear-render-objects)

(defun render ()
  ; TODO: It should be possible to move most of this out to setup.
  (gl:clear-color 0 0 1/8 0)
  (gl:clear-depth 1)
  (gl:depth-func :less)
  (gl:clear :color-buffer :depth-buffer)

  #+nil
  (iter
    (for obj in-vector *render-objects*)
    (format t "prog = ~a~%" (object/program obj))
    (gl:use-program (object/program obj))
    (gl:draw-arrays :triangles 0 (length (object/vertices obj)))))

(defvar *projection-matrix* (identity-matrix 3))
(defvar *view-matrix* (identity-matrix 3))

(defun set-viewport (width height)
  (gl:viewport 0 0 width height)
  (let ((fov-y (degrees-radians 60))
        (z-near 1)
        (z-far 100))
    (setf *projection-matrix* (perspective-matrix fov-y (/ width height) z-near z-far))))

(glfw:def-key-callback quit-on-escape (window key scancode action mod-keys)
  (declare (ignore window scancode))
  (format t "got key ~s ~s ~s~%" mod-keys key action)
  (when (and (eq key :escape) (eq action :press))
    (glfw:set-window-should-close)))

(glfw:def-window-size-callback update-viewport (window w h)
  (declare (ignore window))
  (set-viewport w h))



(defmodule render
  (wrappers
    (with-body-in-main-thread ())
    (glfw:with-init-window (:title "game" :width 800 :height 600
                            :context-version-major 3 :context-version-minor 3
                            :opengl-profile :opengl-core-profile)))
  (setup
    (setf %gl:*gl-get-proc-address* #'glfw:get-proc-address)
    (glfw:set-key-callback 'quit-on-escape)
    (glfw:set-window-size-callback 'update-viewport)
    (set-viewport 800 600)
    (add-render-objects (load-assets-from #p"assets/tri.lisp")))
  (stop-conditions
    (glfw:window-should-close-p))
  (body
    (render)
    (glfw:swap-buffers)
    (glfw:poll-events)))
