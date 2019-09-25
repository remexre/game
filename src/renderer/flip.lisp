(in-package :renderer)

(defcfun "renderer_flip" :void
  (state renderer-state))

(defun flip (renderer)
  (check-type renderer renderer)
  (renderer-flip (pointer renderer)))
