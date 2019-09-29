(in-package :assets)

(defclass prefab ()
  ((tree :initarg :tree :reader prefab-tree :type render-tree)))

(wadler-pprint:def-pretty-object prefab (:print-object t)
  (tree))

(defun load-prefab (path &key (renderer *renderer*))
  (unless (pathnamep path)
    (setf path (pathname path)))
  (let ((data (with-open-file (stream path)
                 (cl-json:decode-json stream))))
    (make-prefab data :renderer renderer)))

(defun make-prefab (data &key (renderer *renderer*))
  (dbg data)
  (assert (dbg (string= (assv :type data) "prefab")))
  (make-instance 'prefab :tree (load-tree (assv :tree data) :renderer *renderer*))
  )
