(defparameter *fps-counter* 0)
(defparameter *fps-dt-sum* 0)
(defvar *fps-avg-factor* 10
  "Recalculates the FPS every *FPS-AVERAGING* frames.")

(defun on-loop (dt)
  (incf *fps-counter*)
  (incf *fps-dt-sum* (/ dt))

  (when (eql *fps-counter* *fps-avg-factor*)
    (let ((fps (format nil "~,2f" (/ *fps-dt-sum* *fps-avg-factor*))))
      (setf (renderer-title-field *renderer* "FPS")  fps
            (renderer-title-field *renderer* "Tris") *drawn-triangles*
            *fps-counter*                            0
            *fps-dt-sum*                             0))))
