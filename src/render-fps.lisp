(in-package #:game)

(defvar *fps-frames-since-last-tick* 0)
(defvar *fps-interval* 1)
(defvar *fps-last-tick-seen* 0)

(defun do-fps ()
  (incf *fps-frames-since-last-tick*)
  (let* ((now (get-universal-time))
         (dt (- now *fps-last-tick-seen*)))
    (unless (< dt *fps-interval*)
      (format t "FPS: ~,1f~%" (/ *fps-frames-since-last-tick* dt))
      (setf *fps-frames-since-last-tick* 0)
      (setf *fps-last-tick-seen* now))))

(defmodule fps (body (do-fps)))
