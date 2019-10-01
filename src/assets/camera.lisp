(in-package :assets)

(defstruct camera
  (pos   (error "Must provide POS")   :type (simple-array single-float (3)))
  (up    (error "Must provide UP")    :type (simple-array single-float (3)))
  (front (error "Must provide FRONT") :type (simple-array single-float (3)))
  (near  (error "Must provide NEAR")  :type single-float)
  (far   (error "Must provide FAR")   :type single-float)
  (fov   (error "Must provide FOV")   :type single-float)
  (ortho nil                          :type boolean))

(defun parse-camera (data)
  "Parses a CAMERA object from DATA, as parsed from JSON."
  (let ((camera (make-camera
                  :pos   (to-float-array '(3) (assv :pos data))
                  :up    (to-float-array '(3) (assv :up data))
                  :front (to-float-array '(3) (assv :front data))
                  :near  (assv :near data)
                  :far   (assv :far data)
                  :fov   (assv :fov data))))

    (when (assv :ortho data)
      (setf (camera-ortho camera) t))

    camera))

(defun camera-proj-xform (camera)
  "Returns the transformation associated with the projection matrix of \
   the camera."
  (check-type camera camera)

  (let ((near  (camera-near camera))
        (far   (camera-far  camera))
        (fov   (camera-fov  camera)))
    (to-float-array '(4 4)
      (if (camera-ortho camera)
          (let ((f+n (+ far near))
                (f-n (- far near)))
            `((1.0 0.0         0.0              0.0)
              (0.0 1.0         0.0              0.0)
              (0.0 0.0 ,(/ -2 f-n) ,(- (/ f+n f-n)))
              (0.0 0.0         0.0              1.0)))
          (let ((n+f (+ near far))
                (n-f (- near far))
                (2fn (* 2 far near)))
            `((,near   0.0          0.0          0.0)
              (  0.0 ,near          0.0          0.0)
              (  0.0   0.0 ,(/ n+f n-f) ,(/ 2fn n-f))
              (  0.0   0.0         -1.0          0.0)))))))

(defun camera-view-xform (camera)
  "Returns the transformation associated with the view matrix of the camera."
  (check-type camera camera)

  (let ((pos   (camera-pos   camera))
        (up    (camera-up    camera))
        (front (camera-front camera)))
    +identity-xform+))
