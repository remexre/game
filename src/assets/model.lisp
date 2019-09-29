(in-package :assets)

(defclass model (asset)
  ((buffer :accessor buffer :initarg :buffer :initform (error "Must provide BUFFER")
           :type immutable-buffer)))

(wadler-pprint:def-pretty-object model (:print-object t)
  (path buffer))

(defun load-model (path &key (renderer *renderer*))
  (unless (pathnamep path)
    (setf path (pathname path)))
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (let ((data (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (read-sequence data stream)
      (make-immutable-buffer renderer data :bytes t))))
