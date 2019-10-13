(in-package :game)

(defun move (forward right up &key (camera (renderer-camera *renderer*)) (move-speed 0.25))
  (let* ((forward (vec3-float-mul (camera-front camera) (* forward move-speed)))
         (up      (vec3-float-mul (camera-up    camera) (* up      move-speed)))
         (right   (vec3-float-mul (camera-right camera) (* right   move-speed)))
         (vecs (list forward up right)))
    (setf (camera-pos camera) (reduce #'vec3-add vecs :initial-value (camera-pos camera)))))

(defun rot (direction amount &key (camera (renderer-camera *renderer*)) (move-speed 10))
  (incf (aref (camera-rot camera) direction) (* amount move-speed)))
