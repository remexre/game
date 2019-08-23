(in-package #:game)

(def-key-callback quit-on-escape (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (when (and (eq key :escape) (eq action :press))
    (set-window-should-close)))

(defun render ()
  (gl:clear :color-buffer)
  (gl:with-pushed-matrix
    (gl:color 1 1 1)
    (gl:rect -25 -25 25 25)))

(defun set-viewport (width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho -50 50 -50 50 -1 1)
  (gl:matrix-mode :modelview)
  (gl:load-identity))

(def-window-size-callback update-viewport (window w h)
  (declare (ignore window))
  (set-viewport w h))



(defmodule render
  (wrappers
    (with-body-in-main-thread ())
    (with-init-window (:title "game" :width 600 :height 800)))
  (setup
    (setf %gl:*gl-get-proc-address* #'get-proc-address)
    (set-key-callback 'quit-on-escape)
    (set-window-size-callback 'update-viewport)
    (gl:clear-color 0 0 0 0)
    (set-viewport 600 400))
  (stop-conditions
    (window-should-close-p))
  (stages
    (render)
    (swap-buffers)
    (poll-events)))
