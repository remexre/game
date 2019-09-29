(in-package :game)

(deftype xform ()
  "A transformation as a 4x4 matrix."
  '(simple-array single-float (4 4)))

(defvar +identity-xform+
  (make-array '(4 4) :element-type 'single-float
              :initial-contents
              (list (list 1.0 0.0 0.0 0.0)
                    (list 0.0 1.0 0.0 0.0)
                    (list 0.0 0.0 1.0 0.0)
                    (list 0.0 0.0 0.0 1.0))))

(check-type +identity-xform+ xform)

(defun compose-xforms (&rest xforms)
  (let ((out +identity-xform+))
    (iter
      (for xform in xforms)
      (setf out (compose-xforms-1 out xform)))
    out))

; TODO: At some point, it'll be faster to go out to a library...
(defun compose-xforms-1 (m1 m2)
  (declare (optimize (speed 3))
           (type (simple-array single-float (4 4)) m1 m2))

  #.(labels ((ele (i j) `(+ (* (aref m1 ,i 0) (aref m2 0 ,j))
                            (* (aref m1 ,i 1) (aref m2 1 ,j))
                            (* (aref m1 ,i 2) (aref m2 2 ,j))
                            (* (aref m1 ,i 3) (aref m2 3 ,j))))
             (row (i) (list 'list (ele i 0) (ele i 1) (ele i 2) (ele i 3)))
             (matrix () (list 'list (row 0) (row 1) (row 2) (row 3))))
      `(make-array '(4 4) :element-type 'single-float :initial-contents ,(matrix))))
