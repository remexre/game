(in-package :game)

(def-loop-init :renderer ()
  (setf *renderer* (init-renderer)))

(defvar *camera-pos*)

(defgeneric draw (object)
  (:method (object)
   (prn :todo "TODO: draw ~a" object)))

(def-loop-body :renderer ()
  (when-let (scene (cdr (renderer-scene-entry *renderer*)))
    (let ((clear-color (scene-clear-color scene))
          (camera      (scene-camera      scene))
          (node        (scene-node        scene)))
      (clear clear-color)
      (setf *drawn-triangles* 0)
      (let ((*camera-pos*  (camera-pos        camera))
            (*shader-proj-xform* (camera-proj-xform camera))
            (*shader-view-xform* (camera-view-xform camera)))
        (draw node))))
  (flip *renderer*))



(defmethod draw ((prefab prefab))
  (draw (prefab-node prefab)))



(defmethod draw ((node node-include-prefab))
  (draw (cdr (node-include-prefab-entry node))))

(defmethod draw ((node node-lod-branch))
  (let* ((branch-distance (node-lod-branch-distance node))  
         (closer          (node-lod-branch-closer   node))  
         (further         (node-lod-branch-further  node))
         (draw-position   (apply-xform-unit-w *shader-model-xform*))
         (distance        (vec3-magnitude (vec3-add *camera-pos* (vec4-to-vec3 draw-position)))))
    (draw (if (< distance branch-distance) closer further))))

(defmethod draw ((node node-model))
  (let* ((entry (node-model-entry node))
         (buf (model-buf (cdr entry))))
    (draw-object buf :cull-radius 1.0)))

(defmethod draw ((node node-multi))
  (iter
    (for child in (node-multi-children node))
    (draw child)))

(defmethod draw ((node node-shader-params))
  (let ((child            (node-shader-params-child   node))
        (*shader-ambient* (node-shader-params-ambient node)) 
        (*shader-diffuse* (node-shader-params-diffuse node)))
    (draw child)))

(defmethod draw ((node node-xform))
  (let ((child  (node-xform-child  node))
        (matrix (node-xform-matrix node))
        (*shader-model-xform* *shader-model-xform*))
    (xform-composef *shader-model-xform* matrix)
    (draw child)))
