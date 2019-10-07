(in-package :game)

(defparameter *fps-counter* 0)
(defparameter *fps-dt-sum* 0)
(defvar *fps-avg-factor* 10
  "Recalculates the FPS every *FPS-AVERAGING* frames.")

(def-loop-body :fps (dt)
  (incf *fps-counter*)
  (incf *fps-dt-sum* (/ dt))

  (when (eql *fps-counter* *fps-avg-factor*)
    (let ((title (format nil "FPS: ~,2f, Tris: ~d" (/ *fps-dt-sum* *fps-avg-factor*)
                         *drawn-triangles*)))
      (setf (title *renderer*) title
            *fps-counter*      0
            *fps-dt-sum*       0))))
