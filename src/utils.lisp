(in-package #:game)

(defun aget (item alist &key key)
  (cdr (assoc item alist :key key)))

(defun pprint-object-pair (stream pair colonp atsignp)
  (declare (ignore colonp atsignp))
  (format stream ":~(~w~) ~w" (car pair) (cdr pair)))

(defun pprint-object (stream name obj slots)
  (format stream "#<~@<~(~w~)~{ ~_~/pprint-object-pair/~}~:>>" name slots))

(defun pprint-object-with-slots (stream obj slots)
  (pprint-object stream (class-name (class-of obj)) obj
                 (mapcar #'(lambda (slot) (cons slot (slot-value obj slot))) slots)))

(defun swank ()
  (load #p"~/.local/share/nvim/plugged/slimv/slime/start-swank.lisp"))
