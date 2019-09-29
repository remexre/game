(in-package :assets)

(defclass prefab (asset)
  ((tree :accessor tree :initarg :tree :type render-tree)))

(wadler-pprint:def-pretty-object prefab (:print-object t)
  (path tree))

(defun load-prefab (path &key (renderer *renderer*))
  (unless (pathnamep path)
    (setf path (pathname path)))
  (let* ((data (with-open-file (stream path)
                 (cl-json:decode-json stream)))
         (tree (load-tree (assv :render data) :renderer renderer)))
    (make-instance 'prefab :path path :tree tree)))
