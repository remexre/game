(in-package :game)

(def-loop-body :drain-events ()
  (iter
    (for event in *events*)
    (prn :events "~s" event))
  (setf *events* nil))

(def-loop-body :foo ()
  #+nil
  (prn t "~a" (load-model *renderer* #p"assets/models/teapot-low-poly.vx")))

(defun main ()
  (enable-loop-stages :events :drain-events :renderer :fps :foo)
  (main-loop))
