(in-package :game)

(def-loop-body :events ()
  (dolist (event (get-events))
    (match event
      (:close-requested (setf *continue-loop* nil))
      ((list :resize w h)
       (gl:viewport 0 0 w h)
       (let ((camera (renderer-camera *renderer*))
             (aspect-ratio (coerce (/ w h) 'single-float)))
         (setf (camera-aspect-ratio camera) aspect-ratio)))
      (_ (handle-event event)))))

(defun handle-event (event)
  (let ((scene (renderer-scene *renderer*)))
    (iter
      (for script-entry in (scene-script-entries scene))
      (for script = (cdr script-entry))
      (script/on-event script event))))
