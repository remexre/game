(in-package #:game)

(defvar *main-loop-modules* nil)

(defmacro defmodule (name &rest forms)
  (if (assoc name *main-loop-modules*)
    (setf (cdr (assoc name *main-loop-modules*)) forms)
    (push (cons name forms) *main-loop-modules*))
  nil)

(defun main-loop-all (key)
  (iter (for (name . vals) in *main-loop-modules*)
        (appending (aget key vals))))

(defmacro main-loop ()
  (let ((body            (main-loop-all 'stages))
        (stop-conditions (main-loop-all 'stop-conditions))
        (setup           (main-loop-all 'setup))
        form)
    (setf form
      `(progn
         ,@setup
         (iter (until (or ,@stop-conditions))
               (progn ,@body))))
    (iter (for wrapper in (nreverse (main-loop-all 'wrappers)))
          (setf form (append wrapper (list form))))
    form))
