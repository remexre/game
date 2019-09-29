(in-package :assets)

(defclass prefab ()
  ((tree :initarg :tree :reader prefab-tree :type render-tree)))

(def-pretty-object prefab (:print-object t)
  (tree))

(defun load-prefab (path)
  (unless (pathnamep path)
    (setf path (pathname path)))
  (let ((data (with-open-file (stream path)
                 (cl-json:decode-json stream))))
    (make-prefab data)))

(defun make-prefab (data)
  (dbg data)
  (assert (dbg (string= (assv :type data) "prefab")))
  (make-instance 'prefab :tree (load-tree (assv :tree data)))
  )
