(in-package :game)

(defparameter *loop-stages* nil)

(defun main-loop-1 ()
  (mapc #'funcall *loop-stages*))

(defun main-loop ()
  (loop (main-loop-1)))
