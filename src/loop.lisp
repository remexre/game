(in-package #:game)

(defvar *main-loop-modules* nil)

(defmacro defmodule (name-ast &rest forms-asts)
  (with-gensyms (entry forms name)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
      (let* ((,forms ',forms-asts)
             (,name ',name-ast)
             (,entry (assoc ,name *main-loop-modules*)))
        (if ,entry
          (setf (cdr ,entry) ,forms)
          (push (cons ,name ,forms) *main-loop-modules*))))))

(defun main-loop-all (key)
  (iter (for (name . vals) in *main-loop-modules*)
        (appending (aget* key vals))))

(defmacro main-loop ()
  (let* ((setup (main-loop-all 'setup))
         (stop (main-loop-all 'stop-conditions))
         (body
          (iter
            (for (name . vals) in *main-loop-modules*)
            (for skip = (intern (format nil "*SKIP-~:@(~a~)*" name)))
            (nconc setup (list `(defvar ,skip nil)))
            (collect `(restart-case (unless ,skip ,@(aget* 'body vals))
                       (skip () :report "Continue the game loop.")
                       (disable () :report ,(format nil "Disable ~a until you (setf ~s nil)" name skip)
                         (setf ,skip t))))))
         (module-names (mapcar #'car *main-loop-modules*))
         (form `(progn
                  (format t "Starting modules: ~a~%" ',module-names)
                  ,@setup
                  (iter
                    (until (or ,@stop))
                    (progn ,@body)))))
    (iter
      (for wrapper in (nreverse (main-loop-all 'wrappers)))
      (setf form (append wrapper (list form))))
    (format t "~&;; Using modules: ~a~%" module-names)
    form))
