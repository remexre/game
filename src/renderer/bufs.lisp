(in-package :renderer)

(defvar *live-buffers* 0)

(defclass immutable-buffer ()
  ((len :initarg :length :reader buffer-length :type fixnum)
   (vbo :initarg :vbo :reader vbo :type fixnum)))

(defun make-immutable-buffer (data)
  (check-type data (vector single-float))

  (let* ((vbo (gl:gen-buffer))
         (free (lambda ()
                 (decf *live-buffers*)
                 (gl:delete-buffers (list vbo)))))
    (incf *live-buffers*)

    (gl:bind-buffer :array-buffer vbo)
    (bracket (arr (gl:alloc-gl-array :float (length data))
                  (gl:free-gl-array arr))
      (iter
        (for i below (length data))
        (setf (gl:glaref arr i) (aref data i)))
      (gl:buffer-data :array-buffer :static-draw arr))
    
    (let ((buffer (make-instance 'immutable-buffer :length (length data) :vbo vbo)))
      (finalize buffer free)
      buffer)))
