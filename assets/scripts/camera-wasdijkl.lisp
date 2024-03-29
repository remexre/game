(defun-match on-event (event)
  ((list :keyboard :w (or :press :repeat) nil) (camera-move  1.0  0.0  0.0))
  ((list :keyboard :s (or :press :repeat) nil) (camera-move -1.0  0.0  0.0))
  ((list :keyboard :a (or :press :repeat) nil) (camera-move  0.0 -1.0  0.0))
  ((list :keyboard :d (or :press :repeat) nil) (camera-move  0.0  1.0  0.0))
  ((list :keyboard :q (or :press :repeat) nil) (camera-move  0.0  0.0 -1.0))
  ((list :keyboard :e (or :press :repeat) nil) (camera-move  0.0  0.0  1.0))
  ((list :keyboard :i (or :press :repeat) nil) (camera-rotate 0 -1.0))
  ((list :keyboard :k (or :press :repeat) nil) (camera-rotate 0  1.0))
  ((list :keyboard :j (or :press :repeat) nil) (camera-rotate 1 -1.0))
  ((list :keyboard :l (or :press :repeat) nil) (camera-rotate 1  1.0))
  ; We ignore the releases of the movement keys.
  ((list :keyboard (or :w :s :a :d :q :e :i :k :j :l) :release nil) t))
