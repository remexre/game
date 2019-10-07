(in-package :assets)

(defstruct prefab
  (node (error "Must provide NODE") :type node))

(defmethod asset-kind ((prefab prefab))
  (declare (ignore prefab))
  :prefab)

(defmethod load-asset ((kind (eql :prefab)) path &key get-entry ignore-cache)
  (declare (ignore get-entry ignore-cache))

  (let ((data (read-json-file path)))
    (assert (string= (assv :type data) "prefab"))
    (make-prefab :node (parse-node (assv :node data)))))
