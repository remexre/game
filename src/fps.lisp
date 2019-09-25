(in-package :game)

(defvar *last-update-time* 0)
(defvar *frames-since-last-update* 0)

(def-loop-body :fps ()
  (let* ((now (get-internal-run-time))
         (delta-ticks (- now *last-update-time*)))
    (incf *frames-since-last-update*)
    (when (> delta-ticks internal-time-units-per-second)
      (setf (renderer:title *renderer*) (format nil "FPS: ~,2f" *frames-since-last-update*)
            *frames-since-last-update* 0
            *last-update-time* now))))
