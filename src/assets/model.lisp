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

(defun load-txt-model (path)
  (let ((buf (make-array '(1024) :element-type 'single-float :adjustable t :fill-pointer 0)))
    (with-open-file (stream path)
      (iter
        (for line = (read-line stream nil))
        (while line)
        (setf line (string-trim '(#\space #\tab #\newline) line))
        (when (string= line "")
          (next-iteration))
        (vector-push-extend (parse-float line) buf)))
    (make-model :buf (make-immutable-buffer buf))))

(defun load-vx-model (path)
  (make-model
    :buf (make-immutable-buffer
           (with-open-file (stream path :element-type '(unsigned-byte 8))
             (let ((buf (make-array (list (file-length stream)) :element-type '(unsigned-byte 8))))
               (read-sequence buf stream)
               buf)) 
           :bytes t)))
