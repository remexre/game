(in-package #:game)

(defun test ()
  (add-render-objects (load-assets-from #p"assets/tri.lisp")))

(defmodule test (setup (test)))

(defun main ()
  (setf *log-caller* nil)
  (push :gl *log-targets*)
  (main-loop))
