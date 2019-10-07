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
  (ambient (to-float-array '(3) '(0.1 0.1 0.1)) :type (simple-array single-float (3)))
  (diffuse (to-float-array '(3) '(1.0 1.0 1.0)) :type (simple-array single-float (3)))
  (child   (error "Must provide CHILD")         :type node))

(defstruct node-xform
  (matrix +identity-xform+             :type xform)
  (child  (error "Must provide CHILD") :type node))

(defun parse-node (data)
  "Parses a NODE object from DATA, as parsed from JSON."
  (let (node)
    (eswitch ((assv :type data) :test string=)
      ("lod-branch"
       (setf node (make-node-lod-branch
                    :distance (assv :distance data)  
                    :closer   (parse-node (assv :closer  data))  
                    :further  (parse-node (assv :further data)))))
      ("model"
       (setf node (make-node-model :entry (load-asset :model (assv :path data) :get-entry t))))
      ("shader-params"
       (setf node (make-node-shader-params :child (parse-node (assv :child data))))

       (when-let (ambient (assv :ambient data))
         (setf (node-shader-params-ambient node) (to-float-array '(3) ambient))) 

       (when-let (diffuse (assv :diffuse data))
         (setf (node-shader-params-diffuse node) (to-float-array '(3) diffuse))))
      ("xform"
       (setf node (make-node-xform :child (parse-node (assv :child data))))

       (when-let (xlat (assv :xlat data))
         (xform-composef (node-xform-matrix node) (xform-xlat xlat)))

       (when-let (rot (assv :rot data))
         (xform-composef (node-xform-matrix node) (xform-rot rot))) 

       (when-let (scale (assv :scale data))
         (xform-composef (node-xform-matrix node) (xform-scale scale)))))
    node))
