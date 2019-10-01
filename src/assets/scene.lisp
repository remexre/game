(in-package :assets)

(defstruct scene
  (camera      (error "Must provide CAMERA")      :type camera)
  (clear-color (error "Must provide CLEAR-COLOR") :type (simple-array single-float (4)))
  (children    (error "Must provide CHILDREN")    :type list))

(defmethod asset-kind ((scene scene))
  (declare (ignore scene))
  :scene)

(defmethod load-asset ((kind (eql :scene)) path &key get-entry ignore-cache)
  (declare (ignore get-entry ignore-cache))

  (let ((data (read-json-file path)))
    (assert (string= (assv :type data) "scene"))
    (make-scene
      :camera (parse-camera (assv :camera data))
      :clear-color (to-float-array '(4) (assv :clear-color data))
      :children (mapcar #'parse-node (assv :children data)))))
