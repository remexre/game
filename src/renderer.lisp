(in-package :game)

(defvar *renderer*)

(def-loop-init :renderer ()
  (setf *renderer* (renderer:make-renderer)))

(def-loop-body :renderer ()
  (sleep 0.1)
  (renderer:flip *renderer*))
