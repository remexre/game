(in-package :game-util)

(defun reload ()
  (asdf:load-system :game)
  (cffi:load-foreign-library 'renderer::renderer)
  nil)

(defun reload-loop ()
  (swank:create-server :dont-close t)
  todo)
