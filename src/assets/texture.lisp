(in-package :assets)

(defstruct texture
  (img (error "Must provide IMG") :type immutable-image))

(defmethod asset-kind ((texture texture))
  (declare (ignore texture))
  :texture)

(defmethod load-asset ((kind (eql :texture)) path &key get-entry ignore-cache)
  (declare (ignore get-entry ignore-cache))

  (eswitch ((pathname-type path) :test #'string=)
    ("png" (load-png-texture path))))

(defun load-png-texture (path)
  (make-texture :img (make-immutable-image-from-png path)))
