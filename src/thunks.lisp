(in-package #:game)

(defvar *thunk-queue* (make-array '(5) :adjustable t :fill-pointer 0))
(defvar *thunk-lock* (make-lock "thunk"))

(defun add-thunk-function (thunk)
  (with-lock-held (*thunk-lock*)
    (vector-push-extend thunk *thunk-queue*)
    nil))

(defmacro add-thunk (&body body)
  `(add-thunk-function (lambda () ,@body)))

(defun do-thunks ()
  (with-lock-held (*thunk-lock*)
    (iter
      (for thunk in-vector *thunk-queue*)
      (funcall thunk))
    (setf (fill-pointer *thunk-queue*) 0)))

(defmodule thunks
  (body (do-thunks)))
