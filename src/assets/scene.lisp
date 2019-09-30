(in-package :assets)

(defstruct camera
  (pos   (error "Must provide POS")   :type (simple-array single-float (3)))
  (up    (error "Must provide UP")    :type (simple-array single-float (3)))
  (front (error "Must provide FRONT") :type (simple-array single-float (3)))
  (near  (error "Must provide NEAR")  :type single-float)
  (far   (error "Must provide FAR")   :type single-float)
  (fov   (error "Must provide FOV")   :type single-float)
  (ortho nil                          :type boolean))

(defstruct scene
  (camera      (error "Must provide CAMERA")      :type camera)
  (clear-color (error "Must provide CLEAR-COLOR") :type (simple-array single-float (4)))
  (children    (error "Must provide CHILDREN")    :type list))

(defmethod asset-kind ((scene scene))
  (declare (ignore scene))
  :scene)

(defmethod load-asset ((kind (eql :scene)) path &key get-entry ignore-cache)
  (let ((data (read-json-file path)))
    (assert (string= (assv :type data) "scene"))
    (make-scene
      :camera (parse-camera (assv :camera data))
      :clear-color (to-float-array '(4) (assv :clear-color data))
      :children (mapcar #'parse-node (assv :children data)))))

(defun parse-camera (data)
  "Parses a CAMERA object from DATA, as parsed from JSON."
  (let ((camera (make-camera
                  :pos   (to-float-array '(3) (assv :pos data))
                  :up    (to-float-array '(3) (assv :up data))
                  :front (to-float-array '(3) (assv :front data))
                  :near  (assv :near data)
                  :far   (assv :far data)
                  :fov   (assv :fov data))))

    (when (assv :ortho data)
      (setf (camera-ortho camera) t))

    camera))

(defun camera-proj-xform (camera)
  "Returns the transformation associated with the projection matrix of \
   the camera."
  (check-type camera camera)

  (let ((mat (if (camera-ortho camera)
                 '()
                 '()
                 )))
    (to-float-array '(4 4) mat)))

(defun camera-view-xform (camera)
  "Returns the transformation associated with the view matrix of the camera."
  (check-type camera camera)

  (todo 'camera-view-xform))
