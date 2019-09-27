(in-package :assets)

(defclass prefab ()
  ((path     :accessor asset-path     :initarg :path     :initform nil :type (or null pathname))
   (renderer :accessor asset-renderer :initarg :renderer               :type renderer)
   (tree     :accessor tree           :initarg :tree                   :type render-tree)))

(defun load-prefab (path &key format (renderer *renderer*))
  (unless (pathnamep path)
    (setf path (pathname path)))
  (unless format
    (setf format (intern (string-upcase (pathname-type path)) :keyword)))

  (ecase format
    ((:json :pv)
     (let* ((data (with-open-file (stream path)
                    (cl-json:decode-json stream)))
            (tree (load-tree (assv :render data) :renderer renderer)))
       (make-instance 'prefab :path path :renderer renderer :tree tree)))))

(defun load-tree (tree &key (renderer *renderer*))
  (princ tree)
  )
