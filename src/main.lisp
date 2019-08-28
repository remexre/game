(in-package #:game)

(defun main ()
  (setf *log-caller* nil)
  (push :gl *log-targets*)
  (main-loop))
