(in-package :assets)

(deftype node ()
  '(or node-include-prefab node-lod-branch node-model node-shader-params node-xform))

(defstruct node-include-prefab
  (entry (error "Must provide ENTRY") :type (cons pathname prefab)))

(defstruct node-lod-branch
  (distance (error "Must provide DISTANCE") :type single-float)
  (closer   (error "Must provide CLOSER")   :type node)
  (further  (error "Must provide FURTHER")  :type node))

(defstruct node-model
  (entry (error "Must provide ENTRY") :type (cons pathname model)))

(defstruct node-shader-params
  (diffuse (to-float-array '(4) '(1.0 1.0 1.0 1.0)) :type (simple-array single-float (4)))
  (child   (error "Must provide CHILD")             :type node))

(defstruct node-xform
  (matrix +identity-xform+             :type xform)
  (child  (error "Must provide CHILD") :type node))

(defun parse-node (data)
  "Parses a NODE object from DATA, as parsed from JSON."
  (let (node)
    (eswitch ((assv :type data) :test string=)
      ("model"
       (setf node (make-node-model :entry (load-asset :model (assv :path data) :get-entry t))))
      ("shader-params"
       (setf node (make-node-shader-params :child (parse-node (assv :child data))))

       (when-let (diffuse (assv :diffuse data))
         (setf (node-shader-params-diffuse node) (to-float-array '(4) diffuse))))
      ("xform"
       (setf node (make-node-xform :child (parse-node (assv :child data))))

       (when-let (scale (assv :scale data))
         (xform-composef (node-xform-matrix node) (xform-scale xlat))) 

       (when-let (rot (assv :rot data))
         (xform-composef (node-xform-matrix node) (xform-rot xlat))) 

       (when-let (xlat (assv :xlat data))
         (xform-composef (node-xform-matrix node) (xform-xlat xlat)))))
    node))
