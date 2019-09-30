(in-package :renderer)

(defvar *live-buffers* 0)

(defclass immutable-buffer ()
  ((len :initarg :length :reader buffer-length :type fixnum)
   (vbo :initarg :vbo :reader vbo :type fixnum)))

(defun make-immutable-buffer (data &key bytes)
  ; Convert a byte array to a float array, if needed.
  (when bytes
    (check-type data (vector (unsigned-byte 8)))

    (unless (zerop (mod (length data) 4))
      (error "DATA of illegal length ~a" (length data)))

    ; TODO: This is unironically horrible. Does this even work???
    ; Even if it does, it might be nice to fuse it with the loop storing into
    ; arr below. This shouldn't be in a hot path though...
    (cffi:with-foreign-pointer (tmp (length data))
      (iter
        (for i below (length data))
        (setf (cffi:mem-ref tmp :uint8 i) (aref data i)))
      (setf data (make-array (list (/ (length data) 4)) :element-type 'single-float))
      (iter
        (for i below (length data))
        (setf (aref data i) (cffi:mem-ref tmp :float i)))))

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
