(in-package #:game)

(defun pprint-object-pair (stream pair colonp atsignp)
  (declare (ignore colonp atsignp))
  (format stream ":~(~w~) ~w" (car pair) (cdr pair)))

(defun pprint-object (stream name obj slots)
  (format stream "#<~@<~(~w~)~{ ~_~/bootstrap-utils::pprint-object-pair/~}~:>>" name slots))

(defun pprint-object-with-slots (stream obj slots)
  (pprint-object stream (class-name (class-of obj)) obj
                 (mapcar #'(lambda (slot) (cons slot (slot-value obj slot))) slots)))

(defun swank ()
  (load #p"~/.local/share/nvim/plugged/slimv/slime/start-swank.lisp"))
