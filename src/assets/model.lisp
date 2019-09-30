(in-package :assets)

(defstruct model
  (buf (error "Must provide BUF") :type immutable-buffer))

(defmethod asset-kind ((model model))
  (declare (ignore model))
  :model)

(defmethod load-asset ((kind (eql :model)) path &key get-entry ignore-cache)
  (eswitch ((pathname-type path) :test #'string=)
    ("txt" (load-txt-model path))
    ("vx"  (load-vx-model  path))))

(defun next-line (stream)
  (iter
    (for line = (read-line stream nil))
    (unless line
      (finish))
    (setf line (string-trim '(#\space #\tab #\newline) line))
    (when (string= line "")
      (next-iteration))
    (leave line)))

(defun load-txt-model (path)
    (with-open-file (stream path)
      (let* ((len (parse-integer (next-line stream)))
             (buf (make-array (list len) :element-type 'single-float)))
          (iter
            (for i below len)
            (setf (aref buf i) (parse-float (next-line stream))))
          (make-model :buf (make-immutable-buffer buf)))))

(defun load-vx-model (path)
  (make-model
    :buf (make-immutable-buffer
           (with-open-file (stream path :element-type '(unsigned-byte 32))
             (let ((floats (make-array (list (file-length stream)) :element-type 'single-float)))
               (iter
                 (for i from 0 below (file-length stream))
                 (for b = (read-byte stream))
                 (while b)
                 (setf (aref floats i) (ieee-floats:decode-float32 b)))
               floats)))))
