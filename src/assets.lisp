(in-package #:game)

(define-condition missing-asset-field (error)
  ((field :initarg :field :reader missing-asset-field/field))
  (:report (lambda (condition stream)
             (format stream "Missing field ~a." (missing-asset-field/field condition)))))

(defun ensure-asset-field (asset field-name)
  (check-type field-name keyword)
  (or (aget field-name (cddr asset))
      (error 'missing-asset-field :field field-name)))

(defun ensure-asset-field* (asset field-name)
  (check-type field-name keyword)
  (or (aget* field-name (cddr asset))
      (error 'missing-asset-field :field field-name)))

(defun load-vao (array-descriptors)
  (let ((vbo-bindings (make-array (length array-descriptors) :fill-pointer 0)))
    (iter
      (for (loc name type size . data) in array-descriptors)
      (vector-push (cons loc (make-vbo size data :name name :type type)) vbo-bindings))
    (make-vao vbo-bindings)))

(defclass object ()
  ((name     :initarg :name     :reader object/name)
   (program  :initarg :program  :reader object/program)
   (vao      :initarg :vao      :reader object/vao)))

(defmethod print-object ((obj object) stream)
  (pprint-object-with-slots stream obj '(name program vao)))

(defun load-asset-object (asset)
  (make-instance 'object :name (cadr asset) :program (load-program asset)
                 :vao (load-vao (ensure-asset-field* asset :vertices))))

(defun load-asset-from-form (form)
  (ecase (car form)
    (:object (load-asset-object form))))

(defun load-assets-from (path)
  "Loads assets from the file at PATH."
  (let ((*package* (symbol-package :keyword))
        (*read-file-base* (directory-namestring path)))
    (iter
      (for form in-file path)
      (collect (load-asset-from-form form)))))
