(in-package :game)

(def-loop-body :events ()
  (let ((camera (when-let (scene-entry (and *renderer* (renderer-scene-entry *renderer*)))
                  (scene-camera (cdr scene-entry)))))
    (dolist (event (get-events))
      (match event
        (:close-requested (setf *continue-loop* nil))
        ((list :keyboard :r :release (list :control))
         (reload-program))
        ((list :keyboard :r :release (list :shift :control))
         (assets:reload-all-assets)
         (reload-program))
        ((list :resize w h)
         (gl:viewport 0 0 w h)
         (when camera
           (setf (camera-aspect-ratio camera) (coerce (/ w h) 'single-float)))
         (handle-event event))
        ((list :keyboard :w (or :press :repeat) nil) (move  1.0  0.0  0.0 camera))
        ((list :keyboard :s (or :press :repeat) nil) (move -1.0  0.0  0.0 camera))
        ((list :keyboard :a (or :press :repeat) nil) (move  0.0 -1.0  0.0 camera))
        ((list :keyboard :d (or :press :repeat) nil) (move  0.0  1.0  0.0 camera))
        ((list :keyboard :q (or :press :repeat) nil) (move  0.0  0.0 -1.0 camera))
        ((list :keyboard :e (or :press :repeat) nil) (move  0.0  0.0  1.0 camera))
        ((list :keyboard :i (or :press :repeat) nil) (rot -1.0 0 camera))
        ((list :keyboard :k (or :press :repeat) nil) (rot  1.0 0 camera))
        ((list :keyboard :j (or :press :repeat) nil) (rot -1.0 1 camera))
        ((list :keyboard :l (or :press :repeat) nil) (rot  1.0 1 camera))
        ((list :keyboard (or :w :s :a :d :q :e :i :k :j :l) :release nil) nil)
        ((list :keyboard _ _ _)
         (handle-event event))
        (_ (prn t "unknown event ~s" event))))))

(defun handle-event (event)
  (prn :event "~a" event)
  (when-let (scene (and *renderer* (cdr (renderer-scene-entry *renderer*))))
    (iter
      (for script-entry in (scene-script-entries scene))
      (for script = (cdr script-entry))
      (script-on-event script event))))

(defun move (forward right up camera &key (move-speed 0.25))
  (let* ((forward (vec3-float-mul (camera-front camera) (* forward move-speed)))
         (up      (vec3-float-mul (camera-up    camera) (* up      move-speed)))
         (right   (vec3-float-mul (camera-right camera) (* right   move-speed)))
         (vecs (list forward up right)))
    (setf (camera-pos camera) (reduce #'vec3-add vecs :initial-value (camera-pos camera)))))

(defun rot (amount direction camera &key (move-speed 10))
  (incf (aref (camera-rot camera) direction) (* amount move-speed)))
