(in-package :renderer)

(defvar *live-buffers* 0)

(defclass immutable-buffer ()
  ((vbo :initarg :vbo :reader vbo :type fixnum)))

(defun make-immutable-buffer (renderer data &key bytes)
  (check-type renderer renderer)

  (prog (buffer free float-data vbo)
    (cond
      (bytes (check-type data (vector (unsigned-byte 8)))
             (unless (zerop (mod (length data) 4))
               (error "DATA of illegal length ~a" (length data)))
             (setf float-data (make-array (list (/ (length data) 4)) :element-type 'single-float))
             ; TODO: This is unironically horrible.
             (cffi:with-foreign-pointer (tmp (length data))
               (iter
                 (for i below (length data))
                 (setf (cffi:mem-ref tmp :uint8 i) (aref data i)))
               (iter
                 (for i below (/ (length data) 4))
                 (setf (aref float-data i) (cffi:mem-ref tmp :float i)))))
      (t     (check-type data (vector single-float))
             (setf float-data data)))

    (format t "float-data = ~a~%" float-data)

    (setf vbo (gl:gen-buffer))
    (incf *live-buffers*)

    (gl:bind-buffer :array-buffer vbo)
    (gl:buffer-data :array-buffer :static-draw vbo float-data)

    (setf buffer (make-instance 'immutable-buffer :vbo vbo))
    (finalize buffer (lambda ()
                       (decf *live-buffers*)
                       (gl:delete-buffers (list vbo))))
    (return buffer)))
