(in-package :assets)

(defclass model ()
  ((buffer :accessor buffer :initarg :buffer :initform (error "Must provide BUFFER")
           :type immutable-buffer)
   (path :accessor asset-path :initarg :path :initform nil :type (or null pathname))))

(defun load-model (path &key format (renderer *renderer*))
  (unless (pathnamep path)
    (setf path (pathname path)))
  (unless format
    (setf format (intern (string-upcase (pathname-type path)) :keyword)))

  (ecase format
    ((:vx)
     (with-open-file (stream path :element-type '(unsigned-byte 8))
       (let ((data (make-array (file-length stream) :element-type '(unsigned-byte 8))))
         (read-sequence data stream)
         (make-immutable-buffer renderer data :bytes t))))))
