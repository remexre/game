(in-package :gl)

(defmacro with-glfw (&body body)
  `(unwind-protect
    (progn
      (glfw-init)
      ,@body)
    (glfwTerminate)))

(defmacro with-glfw-window (window &body body)
  `(let ((,(car window) (glfw-create-window ,@(cdr window))))
     (unwind-protect (progn ,@body)
       (glfwDestroyWindow ,(car window)))))
