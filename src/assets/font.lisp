(in-package :assets)

(defstruct font
  (texture (error "Must provide TEXTURE") :type (cons pathname texture)))

(defmethod asset-kind ((font font))
  (declare (ignore font))
  :font)

(defmethod load-asset ((kind (eql :font)) path &key get-entry ignore-cache)
  (declare (ignore get-entry ignore-cache))

  (eswitch ((pathname-type path) :test #'string=)
    ("json" (load-font path))))

(defun load-font (path)
  (let ((data (read-json-file path)))
    (assert (string= (assv :type data) "font"))
    (make-font :texture (load-asset :texture (assv :path data) :get-entry t))))
