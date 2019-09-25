(in-package :renderer)

(defclass immutable-buffer ()
  ((pointer :initarg :pointer :reader pointer)))

(defcfun "renderer_alloc_immutable" :pointer
  (state renderer-state)
  (data  (:pointer :float))
  (len   :uint64))

(defcfun "renderer_free_immutable" :void
  (pointer :pointer))

(defun make-immutable-buffer (renderer data)
  (check-type renderer renderer)
  (check-type data (vector single-float))

  (let* ((pointer (with-foreign-pointer (buf (* 4 (length data)) len)
                    (iter
                      (for i below (length data))
                      (setf (mem-ref buf :float i) (aref data i)))
                    (renderer-alloc-immutable (pointer renderer) buf len)))
         (free (lambda () (renderer-free-immutable pointer)))
         (buffer (make-instance 'immutable-buffer :pointer pointer)))
    (finalize buffer free)
    buffer))
