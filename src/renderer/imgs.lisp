(in-package :renderer)

(defvar *live-images* 0)

(defclass immutable-image ()
  ((tex :initarg :tex :reader tex :type fixnum)))

(defun make-immutable-image (data)
  (check-type data (array single-float (* * 3)))

  (let* ((tex (gl:gen-texture))
         (free (lambda ()
                 (decf *live-images*)
                 (gl:delete-texture tex))))
    (incf *live-images*)

    (let ((image (make-instance 'immutable-image :tex tex)))
      (finalize image free)
      (load-immutable-image-data tex data)
      image)))

(defun make-immutable-image-from-png (data)
  (let ((png (png-read:read-png-file data)))
    (dbg png)
    'todo))

(defun load-immutable-image-data (tex data)
  (check-type tex fixnum)
  (check-type data (array single-float (* * 3)))

  (let ((width (array-dimension data 0))
        (height (array-dimension data 1)))
    (gl:bind-texture :texture-2d tex)
    (bracket (arr (gl:alloc-gl-array :float (array-total-size data))
                  (gl:free-gl-array arr))
      (let ((i 0))
        (iter
          (for x below width)
          (iter
            (for y below height)
            (iter
              (for z below 3)
              (setf (gl:glaref arr i) (aref data x y z))
              (incf i)))))
      (gl:tex-image-2d :texture-2d 0 :rgb32f width height 0 :rgb :float arr))))
