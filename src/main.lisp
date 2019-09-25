(in-package :game)

(defun main ()
  (prn t "Stages: ~s" *loop-stages*)
  (enable-loop-stages :fps :renderer)
  (main-loop))
