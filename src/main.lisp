(in-package :game)

(def-loop-body :drain-events ()
  (iter
    (for event in *events*)
    (prn :events "~s" event))
  (setf *events* nil))

(defun main ()
  (enable-loop-stages :events :drain-events :renderer :fps)
  (setf (scene *renderer*) (load-asset :scene #p"assets/scenes/spinning-teapot.json"))
  (reload-all-assets)
  (main-loop))
