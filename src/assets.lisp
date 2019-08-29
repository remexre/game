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

(defun load-uniform (name ty data)
  (declare (ignore name))
  (let ((ctor (ecase ty
                (:float #'make-uniform-float)
                (:vec2  #'make-uniform-vec2)
                (:vec3  #'make-uniform-vec3)
                (:vec4  #'make-uniform-vec4)
                (:int   #'make-uniform-int)
                (:ivec2 #'make-uniform-ivec2)
                (:ivec3 #'make-uniform-ivec3)
                (:ivec4 #'make-uniform-ivec4)
                (:mat2 #'make-uniform-mat2)
                (:mat3 #'make-uniform-mat3)
                (:mat4 #'make-uniform-mat4)
                (:mat3x2 #'make-uniform-mat3x2)
                (:mat4x2 #'make-uniform-mat4x2)
                (:mat2x3 #'make-uniform-mat2x3)
                (:mat4x3 #'make-uniform-mat4x3)
                (:mat2x4 #'make-uniform-mat2x4)
                (:mat3x4 #'make-uniform-mat3x4))))
    (cond
      ((equal data '(:identity))
       (funcall ctor :contents (identity-matrix (ecase ty
                                                  (:mat2 2)
                                                  (:mat3 3)
                                                  (:mat4 4)))))
      (t
       (funcall ctor :contents (coerce data 'vector))))))

(defun load-uniforms (uniforms)
   (iter
     (for (loc name ty . data) in uniforms)
     (collect (cons loc (load-uniform name ty data)))))

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
