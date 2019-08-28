(in-package #:game)

(defun aget (item alist &key key)
  (cadr (assoc item alist :key key)))

(defun aget* (item alist &key key)
  (cdr (assoc item alist :key key)))

(defun float-vector (&rest items)
  (make-array (list (length items))
              :element-type 'single-float
              :initial-contents (iter (for item in items)
                                      (collect (coerce item 'single-float)))))

(defmacro once (&body body)
  (let ((name (make-symbol (format nil "*~a*" (gensym)))))
    `(progn
      (defvar ,name t)
      (declaim (special ,name))
      (when ,name
        (setf ,name nil)
        ,@body))))

(defun pprint-object-pair (stream pair colonp atsignp)
  (declare (ignore colonp atsignp))
  (format stream ":~(~w~) ~w" (car pair) (cdr pair)))

(defun pprint-object (stream name slots)
  (format stream "#<~@<~(~w~)~{ ~_~/game::pprint-object-pair/~}~:>>" name slots))

(defun pprint-object-with-slots (stream obj slots)
  (pprint-object stream (class-name (class-of obj))
                 (mapcar #'(lambda (slot) (cons slot (slot-value obj slot))) slots)))

(defvar *read-file-base* #p"./")
(defun read-file (path)
  (with-open-file (stream (merge-pathnames path *read-file-base*))
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))
