(in-package :game)

(defun main ()
  (prn t "Stages: ~s" *loop-stages*)
  (enable-loop-stage :renderer)
  (main-loop))
