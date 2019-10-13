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
  (let* ((data (png-read:image-data (png-read:read-png-file data)))
         (dims (array-dimensions data))
         (width (array-dimension data 0))
         (height (array-dimension data 1)))

    ; Convert grayscale image to color. Yeah, it'd be nice to instead have
    ; support for grayscale textures...
    (when (eql (length dims) 2)
      (setf dims (list width height 3))
      (let ((arr (make-array dims :element-type '(unsigned-byte 8))))
        (iter (for x below width)
          (iter (for y below height)
            (iter (for z below 3)
              (setf (aref arr x y z) (aref data x y)))))
        (setf data arr)))

    ; Convert the byte-based texture to a float-based one.
    (let ((arr (make-array dims :element-type 'single-float)))
      (iter (for x below width)
        (iter (for y below height)
          (iter (for z below 3)
            (setf (aref arr x y z) (coerce (aref data x y z) 'single-float)))))
      (make-immutable-image arr))))

(defun load-immutable-image-data (tex data)
  (check-type tex fixnum)
  (check-type data (array single-float (* * 3)))

  (let* ((width (array-dimension data 0))
         (height (array-dimension data 1))
         (len (* width height 3))
         (arr (make-array len :element-type 'single-float))
         (i 0))
    ; Flatten the array.
    (iter (for x below width)
      (iter (for y below height)
        (iter (for z below 3)
          (setf (aref arr i) (aref data x y z))
          (incf i))))

    ; Send the texture to the GPU.
    (gl:bind-texture :texture-2d tex)
    (gl:tex-image-2d :texture-2d 0 :rgb32f width height 0 :rgb :float arr)))
