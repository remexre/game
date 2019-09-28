(in-package :assets)

(defclass render-tree ()
  ())

(defclass render-node (render-tree)
  ((model :initarg model :reader node-model)))

(wadler-pprint:def-pretty-object render-node (:print-object t)
  (model))

(defun load-tree (tree &key (renderer *renderer*))
  (case (assv :type tree)
    ((:model)
     (format t "~a~%" tree))))
