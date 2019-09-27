(in-package :game-util)

(defun assv (item alist)
  (cdr (assoc item alist)))
