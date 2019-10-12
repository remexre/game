(in-package :assets)

(defstruct camera
  ; view params
  (pos (error "Must provide POS") :type (simple-array single-float (3)))
  (up  (error "Must provide UP")  :type (simple-array single-float (3)))
  (rot (error "Must provide ROT") :type (simple-array single-float (3)))
  ; projection params
  (near         (error "Must provide NEAR") :type single-float)
  (far          (error "Must provide FAR")  :type single-float)
  (fov          (error "Must provide FOV")  :type single-float)
  (aspect-ratio (coerce 16/9 'single-float) :type single-float)
  (ortho        nil                         :type boolean))

(defun parse-camera (data)
  "Parses a CAMERA object from DATA, as parsed from JSON."
  (let ((camera (make-camera
                  :pos  (to-float-array '(3) (assv :pos data))
                  :up   (to-float-array '(3) (assv :up  data))
                  :rot  (to-float-array '(3) (assv :rot data))
                  :near (assv :near data)
                  :far  (assv :far data)
                  :fov  (assv :fov data))))

    (when (assv :ortho data)
      (setf (camera-ortho camera) t))

    camera))

(defun camera-front (camera)
  "Returns the front vector associated with the camera."
  (apply-xform
    (compose-xforms
      (xform-rot-x (aref (camera-rot camera) 0)) 
      (xform-rot-z (aref (camera-rot camera) 2))
      (xform-rot-y (aref (camera-rot camera) 1)))
    (to-float-array '(4) '(0.0 0.0 1.0 0.0))))

(defun camera-right (camera)
  "Returns the right vector associated with the camera."
  (vec3-cross (camera-front camera)
              (camera-up    camera)))

(defun camera-proj-xform (camera)
  "Returns the transformation associated with the projection matrix of \
   the camera."
  (check-type camera camera)

  (let* ((near         (camera-near         camera))
         (far          (camera-far          camera))
         (fov          (camera-fov          camera))
         (aspect-ratio (camera-aspect-ratio camera))
         (angle (/ (deg-to-rad fov) 2))
         (xmax  (* (tan angle) near))
         (ymax  (/ xmax aspect-ratio)))
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
            `((,(/ near xmax)            0.0          0.0          0.0)
              (           0.0 ,(/ near ymax)          0.0          0.0)
              (           0.0            0.0 ,(/ n+f n-f) ,(/ 2fn n-f))
              (           0.0            0.0         -1.0          0.0)))))))

(defun camera-view-xform (camera)
  "Returns the transformation associated with the view matrix of the camera."
  (check-type camera camera)

  (let ((pos (camera-pos camera))
        (rot (camera-rot camera)))
    (compose-xforms
      (xform-rot-x (aref rot 0))
      (xform-rot-z (aref rot 2))
      (xform-rot-y (aref rot 1))
      (xform-xlat pos))))
