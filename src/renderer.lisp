(in-package :game)

(def-loop-init :renderer ()
  (setf *renderer* (make-renderer)))

(defgeneric draw (object xform)
  (:method (object xform)
   (declare (ignore xform))
   (prn :todo "TODO: draw ~a" object)))

(def-loop-body :renderer ()
  (when-let (scene (scene *renderer*))
    (clear (scene-clear-color scene))
    (draw scene +identity-xform+))
  (flip *renderer*))
