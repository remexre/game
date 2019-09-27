(in-package :game)

(def-loop-body :drain-events ()
  (iter
    (for event in *events*)
    (prn :events "~s" event))
  (setf *events* nil))

(defun main ()
  (enable-loop-stages :events :drain-events :renderer :fps)
  (main-loop))

(princ (load-prefab #p"assets/prefabs/teapot.json"))
