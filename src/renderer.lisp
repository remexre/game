(in-package :game)

(def-loop-init :renderer ()
  (setf *renderer* (make-renderer)))

(defgeneric draw (object)
  (:method (object)
   (prn :todo "TODO: draw ~a" object)))

(def-loop-body :renderer ()
  (when-let (scene (scene *renderer*))
    (let ((clear-color (scene-clear-color scene))
          (camera      (scene-camera      scene))
          (children    (scene-children    scene)))
      (clear clear-color)
      (setf *drawn-triangles* 0)
      (let ((*shader-proj* (camera-proj-xform camera))
            (*shader-view* (camera-view-xform camera)))
        (mapc #'draw children))
      ; (prn :perf "Drew ~a triangles")
      ))
  (flip *renderer*))



; node-include-prefab

; node-lod-branch

(defmethod draw ((node node-model))
  (let* ((entry (node-model-entry node))
         (buf (model-buf (cdr entry))))
    (draw-object buf)))

(defmethod draw ((node node-shader-params))
  (let ((child            (node-shader-params-child   node))
        (*shader-diffuse* (node-shader-params-diffuse node)))
    (draw child)))

(defmethod draw ((node node-xform))
  (let ((child  (node-xform-child  node))
        (matrix (node-xform-matrix node))
        (*shader-model* *shader-model*))
    (xform-composef *shader-model* matrix)
    (draw child)))
