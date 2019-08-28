(in-package #:game)

(defun degrees-radians (degs)
  (* pi (/ degs 180.0)))

(defun identity-matrix (size)
  (let ((arr (make-array (list (* size size)) :element-type 'single-float)))
    (iter (for i from 0 to (1- size))
          (setf (aref arr (+ (* i size) i)) 1.0))
    arr))

(defun perspective-matrix (fov-y aspect-ratio z-near z-far)
  (let* ((f             (/ (tan (/ fov-y 2))))
         (f/a           (/ f aspect-ratio))
         (zn+zf/zn-zf   (/ (+ z-near z-far) (- z-near z-far))) 
         (2*zn*zf/zn-zf (/ (* 2 z-near z-far) (- z-near z-far))))
    (float-vector
      f/a  0      0           0
       0   f      0           0
       0   0 zn+zf/zn-zf 2*zn*zf/zn-zf
       0   0     -1           0)))
