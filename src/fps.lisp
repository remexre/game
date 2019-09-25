(in-package :game)

(defparameter *fps-counter* 0)
(defparameter *fps-dt-sum* 0)
(defvar *fps-averaging* 10
  "Recalculates the FPS every *FPS-AVERAGING* frames.")

(def-loop-body :fps (dt)
  (incf *fps-counter*)
  (incf *fps-dt-sum* (/ dt))

  (when (eql *fps-counter* *fps-averaging*)
    (setf (renderer:title *renderer*) (format nil "FPS: ~,2f" (/ *fps-dt-sum* *fps-averaging*))
          *fps-counter* 0
          *fps-dt-sum* 0)))
