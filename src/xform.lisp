(in-package :game)

(deftype xform ()
  "A transformation as a 4x4 matrix."
  '(vector single-float 16))

(defvar +identity-xform+
  #(1.0 0.0 0.0 0.0
    0.0 1.0 0.0 0.0
    0.0 0.0 1.0 0.0
    0.0 0.0 0.0 1.0))

(check-type +identity-xform+ xform)

(defun compose-xforms (&))
