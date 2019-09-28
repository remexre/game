(in-package :game)

(def-loop-body :drain-events ()
  (iter
    (for event in *events*)
    (prn :events "~s" event))
  (setf *events* nil))

(defun main ()
  (enable-loop-stages :events :drain-events :renderer :fps)
  (main-loop))

#+nil
(format t "~a~%" (load-asset #p"assets/prefabs/teapot.json" :kind :prefab))
