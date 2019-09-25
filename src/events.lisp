(in-package :game)

(defvar *events* nil)

(def-loop-body :events ()
  (let ((events (get-events *renderer*)))
    (when (member :close-requested events)
      (setf *continue-loop* nil))
    (setf *events* (nconc events *events*))

    (when (> (length *events*) 10)
      (prn :events "~a events queued!" (length *events*)))))
