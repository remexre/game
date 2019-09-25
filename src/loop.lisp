(in-package :game)

(defvar *loop-stages* nil)
(defvar *loop-stages-enabled* nil)
(defvar *loop-inits* nil)
(defvar *loop-bodies* nil)

(defun main-loop-1 ()
  (iter
    (for name in *loop-stages-enabled*)
    (for body = (cdr (assoc name *loop-bodies*)))
    (when body
      (restart-case (funcall body)
        (continue ()
          :report "Continue running the game loop."
          nil)
        (abort ()
          :report (lambda (stream) (format stream "Disable the ~a stage." name))
          (setf *loop-stages-enabled* (delete name *loop-stages-enabled*)))))))

(defun main-loop ()
  (loop (main-loop-1)))

(defmacro def-loop-init (name () &body body)
  (check-type name keyword)
  `(progn
     (pushnew ,name *loop-stages*)
     (push (cons ,name (lambda () ,@body)) *loop-inits*)
     nil))

(defmacro def-loop-body (name () &body body)
  (check-type name keyword)
  `(progn
     (pushnew ,name *loop-stages*)
     (push (cons ,name (lambda () ,@body)) *loop-bodies*)
     nil))

(defun enable-loop-stage (name)
  (check-type name keyword)
  (unless (member name *loop-stages*)
    (error "No loop stage ~a found; was it defined?" name))
  (when (member name *loop-stages-enabled*)
    (error "Loop stage ~a was already enabled" name))
  (push name *loop-stages-enabled*) 
  (let ((init (cdr (assoc name *loop-inits*))))
    (when init
      (funcall init)))
  nil)

(defun enable-loop-stages (&rest names)
  (mapc #'enable-loop-stage names)
  nil)
