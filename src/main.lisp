(in-package :game)

(defun main ()
  (enable-loop-stages :events :renderer :fps)
  (main-loop))
