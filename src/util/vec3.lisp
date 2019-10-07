(in-package :game-util)

(defun vec3-cross (a b)
  (declare (optimize (speed 3))
           (type (simple-array single-float (3)) a b))

  (let ((a1 (aref a 0))
        (a2 (aref a 1))
        (a3 (aref a 2))
        (b1 (aref b 0))
        (b2 (aref b 1))
        (b3 (aref b 2)))
    (to-float-array '(3)
      (list
        (- (* a2 b3) (* a3 b2))
        (- (* a3 b1) (* a1 b3))
        (- (* a1 b2) (* a2 b1))))))

(defun vec3-magnitude (v)
  (declare (optimize (speed 3))
           (type (simple-array single-float (3)) v))
  (let ((x (aref v 0))
        (y (aref v 1))
        (z (aref v 2)))
    (sqrt (+ (* x x) (* y y) (* z z)))))

(defun vec3-normalize (v)
  (declare (optimize (speed 3))
           (type (simple-array single-float (3)) v))
  (let ((mag (vec3-magnitude v)))
    (map '(simple-array single-float (3))
         (lambda (x) (/ x mag))
         v)))

(defun vec3-add (a b)
  (declare (optimize (speed 3))
           (type (simple-array single-float (3)) a b))
  ; TODO: Does this get unrolled and vectorized?
  (map '(simple-array single-float (3)) #'+ a b))

(defun vec3-float-mul (v x)
  (declare (optimize (speed 3))
           (type (simple-array single-float (3)) v)
           (type single-float x))
  ; TODO: Does this get unrolled and vectorized?
  (map '(simple-array single-float (3)) (lambda (y) (* y x)) v))

(defun vec3-sub (a b)
  (declare (optimize (speed 3))
           (type (simple-array single-float (3)) a b))
  ; TODO: Does this get unrolled and vectorized?
  (map '(simple-array single-float (3)) #'- a b))

(defun vec4-to-vec3 (v)
  (declare (optimize (speed 3))
           (type (simple-array single-float (4)) v))
  (to-float-array '(3)
    (list (aref v 0)
          (aref v 1)
          (aref v 2))))
