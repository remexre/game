(in-package #:game)

(defstruct shader
  (handle (error "Must provide HANDLE") :type fixnum :read-only t)
  (path (error "Must provide PATH") :type pathname :read-only t)
  (type (error "Must provide TYPE") :type keyword :read-only t))

(define-condition shader-compile-error (error)
  ((compile-log :initarg :compile-log :reader shader-compile-error/compile-log)
   (shader-type :initarg :shader-type :reader shader-compile-error/shader-type)
   (path :initarg :path :reader shader-compile-error/path))
  (:report (lambda (condition stream)
             (with-slots (compile-log path) condition
               (format stream "Failed to compile ~a:~%~a" path compile-log)))))

(defun load-shader (shader-type path)
  (let ((handle (gl:create-shader shader-type)))
    (with-simple-restart (retry "Retry compiling shader ~s." path)
      (gl:shader-source handle (read-file path))
      (gl:compile-shader handle)
      (unless (gl:get-shader handle :compile-status)
        (error 'shader-compile-error :compile-log (gl:get-shader-info-log handle) :path path
               :shader-type shader-type)))
    (let ((free (lambda () (add-thunk (gl:delete-shader handle))))
          (shader (make-shader :handle handle :path path :type shader-type)))
      (finalize shader free)
      shader)))

(defstruct program
  (handle (error "Must provide HANDLE") :type fixnum :read-only t)
  (shaders (error "Must provide SHADERS") :type (vector shader) :read-only t))

(define-condition shader-link-error (error)
  ((link-log :initarg :link-log :reader shader-link-error/link-log)
   (shaders :initarg :shaders :reader shader-link-error/shaders))
  (:report (lambda (condition stream)
             (with-slots (link-log shaders) condition
               (format stream "Failed to link ~a:~%~a" shaders link-log)))))

(defun load-program (asset)
  (let* ((handle (gl:create-program))
         (shader-block (ensure-asset-field* asset :shaders))
         (shaders (make-array (length shader-block) :fill-pointer 0)))
    (iter
      (for (shader-type path) in shader-block)
      (let ((shader (load-shader shader-type path)))
        (gl:attach-shader handle (shader-handle shader))
        (vector-push shader shaders)))
    (gl:link-program handle)
    (unless (gl:get-program handle :link-status)
      (error 'shader-link-error :link-log (gl:get-program-info-log handle) :shaders shaders))
    (let ((free (lambda () (add-thunk (gl:delete-program handle))))
          (program (make-program :handle handle :shaders shaders)))
      (finalize program free)
      program)))

(defun use-program (program)
  (gl:use-program (program-handle program)))
