(in-package :assets)

(defclass render-tree ()
  ())

(defclass render-clear-color (render-tree)
  ((r :initarg :r :reader render-clear-color-r :type single-float)
   (g :initarg :g :reader render-clear-color-g :type single-float)
   (b :initarg :b :reader render-clear-color-b :type single-float)
   (a :initarg :a :reader render-clear-color-a :type single-float)))

(wadler-pprint:def-pretty-object render-clear-color (:print-object t)
  (r g b a))

(defclass render-group (render-tree)
  ((children :initarg :children :reader render-group-children :type list)))

(wadler-pprint:def-pretty-object render-group (:print-object t)
  (children))

(defclass render-model (render-tree)
  ((entry :initarg :entry :reader render-entry :type (cons pathname model))))

(wadler-pprint:def-pretty-object render-model (:print-object t)
  (entry))

(defclass render-prefab (render-tree)
  ((entry :initarg :entry :reader render-entry :type (cons pathname prefab))))

(wadler-pprint:def-pretty-object render-prefab (:print-object t)
  (entry))

(defun load-tree (tree &key (renderer *renderer*))
  (eswitch ((assv :type tree) :test string=)
    ("clear-color"
     (make-instance 'render-clear-color :r (assv :r tree) :g (assv :g tree)
                    :b (assv :b tree) :a (assv :a tree))
     )
    ("group"
     (make-instance 'render-group :children
                    (mapcar (lambda (child) (load-tree child :renderer renderer))
                            (assv :children tree))))
    ("include"
     (make-instance 'render-prefab :entry (load-asset-entry (pathname (assv :path tree)))))
    ("model"
     (make-instance 'render-model :entry (load-asset-entry (pathname (assv :path tree)))))
    ))
