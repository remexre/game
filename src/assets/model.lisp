(in-package :assets)

(defstruct model
  (buf (error "Must provide BUF") :type immutable-buffer))

(defmethod asset-kind ((model model))
  (declare (ignore model))
  :model)

(defmethod load-asset ((kind (eql :model)) path &key get-entry ignore-cache)
  (make-model
    :buf (make-immutable-buffer
           (with-open-file (stream path :element-type '(unsigned-byte 8))
             (let ((buf (make-array (list (file-length stream)) :element-type '(unsigned-byte 8))))
               (read-sequence buf stream)
               buf)) 
           :bytes t)))
