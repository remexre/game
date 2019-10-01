(in-package :game)

(defvar *events* nil)

(def-loop-body :events ()
  (dolist (event (get-events))
    (match event
      (:close-requested (setf *continue-loop* nil))
      ((list :keyboard :r :release (list :control))
       (assets:reload-all-assets))
      ((list :keyboard _ _ _)
       (push event *events*))
      (_ (prn t "unknown event ~s" event))))

  (when (> (length *events*) 10)
    (prn :events "~a events queued!" (length *events*))))
