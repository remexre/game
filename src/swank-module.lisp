(in-package #:game)

(defun poll-events ()
  #+sbcl
  (sb-impl::serve-event 0))

(defun update-swank ()
  "Handle REPL requests."
  (poll-events)
  (iter
    (for connection in swank::*connections*)
    (swank::handle-requests connection t)))

(defmodule swank
  (setup
    (setf swank:*communication-style* nil)
    (with-simple-restart (abort "Don't try to start SWANK.")
      (swank:create-server :style :fd-handler :dont-close t)))
  (body
    (update-swank)))
