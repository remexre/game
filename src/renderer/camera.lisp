(in-package :renderer)

(defun renderer-camera (renderer)
  (assets:scene-camera (renderer-scene renderer)))
