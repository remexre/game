(in-package :assets)

(defclass prefab ()
  (
   (path :accessor path :initarg :path :initform nil :type (or null pathname))))

(defun load-prefab (renderer path &key format)
  (unless (pathnamep path)
    (setf path (pathname path)))
  (unless format
    (setf format (intern (string-upcase (pathname-type path)) :keyword)))

  (ecase format
    ((:json :pv)
     (with-open-file (stream path)
       (print (cl-json:decode-json stream))))))
