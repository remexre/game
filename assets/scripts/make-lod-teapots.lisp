(defun load-teapot-at (x y z)
  (parse-node 
    `((:type . "xform")
      (:xlat ,x ,y ,z)
      (:child . ((:type . "include")
                 (:path . "assets/prefabs/lod-teapot.json"))))))

(defun on-load ()
  (check-type *root* node-multi)
  (with-accessors ((nodes node-multi-children)) *root*
    (iter
      (for x from -10.0 to 10.0)
      (iter
        (for y in '(-2.0 2.0))
        (iter
          (for z from -10.0 to 10.0)
          (push (load-teapot-at x y z) nodes))))))
