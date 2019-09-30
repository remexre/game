(in-package :game)

(def-loop-init :renderer ()
  (setf *renderer* (make-renderer))
  #+nil
  (setf (clear-color *renderer*) #(0.06125 0.06125 1.0 1.0)))

(defgeneric before-clear (object)
  (:method (object)
   (declare (ignore object))))

(defgeneric draw (object xform)
  (:method (object xform)
   (declare (ignore xform))
   (prn :todo "TODO: draw ~a" object)))

(def-loop-body :renderer ()
  (when-let (scene (scene *renderer*))
    (before-clear scene)
    (gl:clear :color-buffer :depth-buffer)
    (draw scene +identity-xform+))
  (flip *renderer*))
