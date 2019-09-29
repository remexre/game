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



#+nil
(defmethod draw ((object model) xform)
  (draw (prefab-tree object) xform))



(defmethod draw ((object prefab) xform)
  (draw (prefab-tree object) xform))



(defmethod before-clear ((object render-clear-color))
  (gl:clear-color
    (render-clear-color-r object)
    (render-clear-color-g object)
    (render-clear-color-b object)
    (render-clear-color-a object)))

(defmethod draw ((object render-clear-color) xform)
  (declare (ignore object xform)))



(defmethod before-clear ((object render-group))
  (iter
    (for child in (render-group-children object))
    (before-clear child)))

(defmethod draw ((object render-group) xform)
  (iter
    (for child in (render-group-children object))
    (draw child xform)))



(defmethod before-clear ((object render-prefab))
  (before-clear (cdr (render-entry object))))

(defmethod draw ((object render-prefab) xform)
  (draw (cdr (render-entry object)) xform))



(defmethod before-clear ((object render-model))
  (before-clear (cdr (render-entry object))))

(defmethod draw ((object render-model) xform)
  (draw (cdr (render-entry object)) xform))
