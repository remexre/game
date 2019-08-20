(in-package :gl-mid)

(defun glfw-init ()
  (when (eq (glfwInit) 'gl-raw:error-glfw-init-failed)
    (throw it or something, idk)
    (i guess i should implement the condition system?)))

(defclass window ())

(defun glfw-create-window (&rest args)
  (let ((win (apply #'glfwCreateWindow args)))
    (when (eq win 'gl-raw:error-glfw-create-window-failed)
      (throw it or something, idk)
      (i guess i should implement a condition system?)
      (prolly wont tho))
    win))
