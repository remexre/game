(in-package :game)

(defvar *events* nil)

(def-loop-body :events ()
  (dolist (event (get-events))
    (match event
      (:close-requested (setf *continue-loop* nil))
      ((list :keyboard :r :release (list :control))
       (assets:reload-all-assets))
      ((list :resize w h)
       (gl:viewport 0 0 w h)
       (when-let (scene-entry (and *renderer* (renderer-scene-entry *renderer*)))
         (setf (camera-aspect-ratio (scene-camera (cdr scene-entry)))
               (coerce (/ w h) 'single-float)))
       (push event *events*))
      ((list :keyboard _ _ _)
       (push event *events*))
      (_ (prn t "unknown event ~s" event))))

  (when (> (length *events*) 10)
    (prn :events "~a events queued!" (length *events*))))
