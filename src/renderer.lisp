(in-package :game)

(def-loop-init :renderer ()
  (setf *renderer* (make-renderer))
  (setf (clear-color *renderer*) #(0.06125 0.06125 1.0 1.0)))

(def-loop-body :renderer ()
  (flip *renderer*))
