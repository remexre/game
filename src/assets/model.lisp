(in-package :assets)

(defclass model ()
  ((buffer :accessor buffer :initarg :buffer :initform (error "Must provide BUFFER")
           :type immutable-buffer)
   (path :accessor path :initarg :path :initform nil :type (or null pathname))))

(defun load-model (path &key format)
  (unless (pathnamep path)
    (setf path (pathname path)))
  (unless format
    (setf format (intern (string-upcase (pathname-type path)) :keyword)))

  (ecase format
    ((:vx)
     (with-open-file (stream path :element-type 'unsigned-byte)
       (prn :assets "POG ~s" stream)))))
