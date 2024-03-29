(in-package :assets)

(defstruct scene
  (camera         (error "Must provide CAMERA")         :type camera)
  (clear-color    (error "Must provide CLEAR-COLOR")    :type (simple-array single-float (4)))
  (node           (error "Must provide NODE")           :type node)
  (script-entries (error "Must provide SCRIPT-ENTRIES") :type list))

(defmethod asset-kind ((scene scene))
  (declare (ignore scene))
  :scene)

(defmethod load-asset ((kind (eql :scene)) path &key get-entry ignore-cache)
  (declare (ignore get-entry ignore-cache))

  (let ((data (read-json-file path)))
    (assert (string= (assv :type data) "scene"))
    (let ((scene (make-scene
                   :camera (parse-camera (assv :camera data))
                   :clear-color (to-float-array '(4) (assv :clear-color data))
                   :node (parse-node (assv :node data))
                   :script-entries (mapcar
                                     (lambda (path) (load-asset :script (pathname path)
                                                                :get-entry t))
                                     (assv :scripts data)))))
      (iter
        (for script-entry in (scene-script-entries scene))
        (for script = (cdr script-entry))
        (script/on-load script scene))
      scene)))
