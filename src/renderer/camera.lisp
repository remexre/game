(in-package :renderer)

(defun renderer-camera (renderer)
  (assets:scene-camera (cdr (renderer-scene-entry renderer))))
