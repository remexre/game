(in-package :assets)

(defclass scene ()
  ((prefab :initarg :prefab :reader scene-prefab :type prefab)
   (clear-color :initarg clear-color :reader scene-clear-color
                :type (simple-vector single-float (4)))))

(def-pretty-object scene (:print-object t)
  (prefab clear-color))
