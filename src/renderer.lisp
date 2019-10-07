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
          (children    (scene-children    scene)))
      (clear clear-color)
      (setf *drawn-triangles* 0)
      (let ((*camera-pos*  (camera-pos        camera))
            (*shader-proj* (camera-proj-xform camera))
            (*shader-view* (camera-view-xform camera)))
        (mapc #'draw children))))
  (flip *renderer*))



; node-include-prefab

(defmethod draw ((node node-lod-branch))
  (let* ((branch-distance (node-lod-branch-distance node))  
         (closer          (node-lod-branch-closer   node))  
         (further         (node-lod-branch-further  node))
         (draw-position   (apply-xform *shader-model* (to-float-array '(4) '(0.0 0.0 0.0 1.0))))
         (distance        (vec3-magnitude (vec3-sub *camera-pos* (vec4-to-vec3 draw-position)))))
    (draw (if (< distance branch-distance) closer further))))

(defmethod draw ((node node-model))
  (let* ((entry (node-model-entry node))
         (buf (model-buf (cdr entry))))
    (draw-object buf)))

(defmethod draw ((node node-shader-params))
  (let ((child            (node-shader-params-child   node))
        (*shader-ambient* (node-shader-params-ambient node)) 
        (*shader-diffuse* (node-shader-params-diffuse node)))
    (draw child)))

(defmethod draw ((node node-xform))
  (let ((child  (node-xform-child  node))
        (matrix (node-xform-matrix node))
        (*shader-model* *shader-model*))
    (xform-composef *shader-model* matrix)
    (draw child)))
