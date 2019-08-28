(in-package #:game)

(define-condition invalid-asset-field (error)
  ((field :initarg :field :reader missing-asset-field/field)
   (why   :initarg :why   :reader missing-asset-field/why))
  (:report (lambda (condition stream)
             (format stream "Invalid field ~a: ~a." (invalid-asset-field/field condition)
                     (invalid-asset-field/why condition)))))

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

(defun load-uniforms (uniforms)
  (lg t "ignoring uniforms ~a" uniforms)
  nil)

(defun load-vao (array-descriptors)
  (let ((vbo-bindings (make-array (length array-descriptors) :fill-pointer 0)))
    (iter
      (for (loc name type size . data) in array-descriptors)
      (vector-push (cons loc (make-vbo size data :name name :type type)) vbo-bindings))
    (make-vao vbo-bindings)))

(defun load-asset-object (asset)
  (make-instance 'object :name (cadr asset) :program (load-program asset)
                 :uniforms (load-uniforms (ensure-asset-field* asset :uniforms))
                 :vao (load-vao (ensure-asset-field* asset :vertices))))

(defun load-asset-from-form (form)
  (ecase (car form)
    (:object (load-asset-object form))))

(defun load-assets-from (path)
  "Loads assets from the file at PATH."
  (let ((*read-file-base* (directory-namestring path))
        (keyword-read (lambda (&rest args)
                        (let ((*package* (symbol-package :keyword)))
                          (apply #'read args)))))
    (iter
      (for form in-file path using keyword-read)
      (collect (load-asset-from-form form)))))
