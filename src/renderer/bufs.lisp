(in-package :renderer)

(defvar *live-buffers* 0)

(defclass immutable-buffer ()
  ((pointer :initarg :pointer :reader pointer)))

(defcfun "renderer_alloc_immutable" :pointer
  (state renderer-state)
  (data  (:pointer :float))
  (len   :uint64))

(defcfun "renderer_free_immutable" :void
  (pointer :pointer))

(defun make-immutable-buffer (renderer data &key bytes)
  (check-type renderer renderer)
  (cond
    (bytes (check-type data (vector (unsigned-byte 8)))
           (unless (zerop (mod (length data) 4))
             (error "DATA of illegal length ~a" (length data))))
    (t     (check-type data (vector single-float))))

  (let* ((len (* (if bytes 1 4) (length data)))
         (pointer (with-foreign-pointer (buf len)
                    (iter
                      (for i below (length data))
                      (if bytes
                        (setf (mem-ref buf :uint8 i) (aref data i))
                        (setf (mem-ref buf :float i) (aref data i))))
                    (renderer-alloc-immutable (pointer renderer) buf (/ len 4))))
         (free (lambda ()
                 (decf *live-buffers*)
                 (renderer-free-immutable pointer)))
         (buffer (make-instance 'immutable-buffer :pointer pointer)))
    (incf *live-buffers*)
    (finalize buffer free)
    buffer))
