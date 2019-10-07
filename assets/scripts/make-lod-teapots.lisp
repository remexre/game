(defun load-teapot-at (x z)
  (parse-node 
    `((:type . "xform")
      (:xlat ,x -1.0 ,z)
      (:child . ((:type . "include")
                 (:path . "assets/prefabs/lod-teapot.json"))))))

(defun on-load ()
  (check-type *root* node-multi)
  (with-accessors ((nodes node-multi-children)) *root*
    (iter
      (for i from -10.0 to 10.0)
      (iter
        (for j from -10.0 to 10.0)
        (push (load-teapot-at i j) nodes)))))
