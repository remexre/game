(in-package :game-util)

(deftype xform ()
  "A transformation as a 4x4 matrix."
  '(simple-array single-float (4 4)))

(defparameter +identity-xform+
  (make-array '(4 4) :element-type 'single-float
              :initial-contents
              (list (list 1.0 0.0 0.0 0.0)
                    (list 0.0 1.0 0.0 0.0)
                    (list 0.0 0.0 1.0 0.0)
                    (list 0.0 0.0 0.0 1.0))))

(check-type +identity-xform+ xform)

(defun apply-xform (m v)
  "mat4 * vec4"
  (declare (optimize (speed 3))
           (type (simple-array single-float (4 4)) m)
           (type (simple-array single-float (4)) v))
  
  #.(labels ((ele (i) `(+ (* (aref m ,i 0) (aref v 0))  
                          (* (aref m ,i 1) (aref v 1))  
                          (* (aref m ,i 2) (aref v 2))  
                          (* (aref m ,i 3) (aref v 3))))
             (vec () (list 'list (ele 0) (ele 1) (ele 2) (ele 3))))
      `(make-array '(4) :element-type 'single-float :initial-contents ,(vec))))

(defun compose-xforms (&rest xforms)
  (let ((out +identity-xform+))
    (iter
      (for xform in xforms)
      (setf out (compose-xforms-1 out xform)))
    out))

; TODO: At some point, it'll be faster to go out to a library...
(defun compose-xforms-1 (m1 m2)
  "mat4 * mat4"
  (declare (optimize (speed 3))
           (type (simple-array single-float (4 4)) m1 m2))

  #.(labels ((ele (i j) `(+ (* (aref m1 ,i 0) (aref m2 0 ,j))
                            (* (aref m1 ,i 1) (aref m2 1 ,j))
                            (* (aref m1 ,i 2) (aref m2 2 ,j))
                            (* (aref m1 ,i 3) (aref m2 3 ,j))))
             (row (i) (list 'list (ele i 0) (ele i 1) (ele i 2) (ele i 3)))
             (matrix () (list 'list (row 0) (row 1) (row 2) (row 3))))
      `(make-array '(4 4) :element-type 'single-float :initial-contents ,(matrix))))

(defmacro xform-composef (place m2)
  `(setf ,place (compose-xforms-1 ,place ,m2)))

(defun flatten-xform (m)
  (declare (optimize (speed 3))
           (type (simple-array single-float (4 4)) m))
  (make-array '(16) :element-type 'single-float :displaced-to m))

(defun xform-rot (v)
  (unless (typep v '(simple-array single-float (3)))
    (setf v (to-float-array '(3) v)))

  (let ((x (aref v 0))
        (y (aref v 1))
        (z (aref v 2)))
    (to-float-array '(4 4)
                    `(( ,x 0.0 0.0 0.0)
                      (0.0  ,y 0.0 0.0)
                      (0.0 0.0  ,z 0.0)
                      (0.0 0.0 0.0 1.0)))))

(defun xform-scale (v)
  (when (typep v 'single-float)
    (setf v (make-array '(3) :element-type 'single-float :initial-element v)))
  (unless (typep v '(simple-array single-float (3)))
    (setf v (to-float-array '(3) v)))

  (let ((x (aref v 0))
        (y (aref v 1))
        (z (aref v 2)))
    (to-float-array '(4 4)
                    `(( ,x 0.0 0.0 0.0)
                      (0.0  ,y 0.0 0.0)
                      (0.0 0.0  ,z 0.0)
                      (0.0 0.0 0.0 1.0)))))

(defun xform-xlat (v)
  (unless (typep v '(simple-array single-float (3)))
    (setf v (to-float-array '(3) v)))

  (let ((x (aref v 0))
        (y (aref v 1))
        (z (aref v 2)))
    (to-float-array '(4 4)
                    `((1.0 0.0 0.0  ,x)
                      (0.0 1.0 0.0  ,y)
                      (0.0 0.0 1.0  ,z)
                      (0.0 0.0 0.0 1.0)))))
