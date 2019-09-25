(in-package :game)

(defvar *loop-stages* nil)
(defvar *loop-stages-enabled* nil)
(defvar *loop-inits* nil)
(defvar *loop-bodies* nil)

(defvar *loop-last-time* 0)

(defun main-loop-1 ()
  (let* ((now (get-internal-run-time))
         (dt (/ (- now *loop-last-time*) internal-time-units-per-second)))
    (setf *loop-last-time* now)
    (iter
      (for name in *loop-stages-enabled*)
      (for body = (cdr (assoc name *loop-bodies*)))
      (when body
        (restart-case (funcall body dt)
          (continue ()
            :report "Continue running the game loop."
            nil)
          (abort ()
            :report (lambda (stream) (format stream "Disable the ~a stage." name))
            (setf *loop-stages-enabled* (delete name *loop-stages-enabled*))))))))

(defun main-loop ()
  (setf *loop-last-time* (get-internal-run-time))
  (loop (main-loop-1)))

(defmacro def-loop-init (name () &body body)
  (check-type name keyword)
  `(progn
     (pushnew ,name *loop-stages*)
     (push (cons ,name (lambda () ,@body)) *loop-inits*)
     nil))

(defmacro def-loop-body (name (&optional (dt (gensym))) &body body)
  (check-type name keyword)
  `(progn
     (pushnew ,name *loop-stages*)
     (push (cons ,name (lambda (,dt) ,@body)) *loop-bodies*)
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

(defun loop-rerun-init (name)
  (unless (member name *loop-stages-enabled*)
    (error "Loop stage ~a was not yet enabled" name))
  (let ((init (cdr (assoc name *loop-inits*))))
    (unless init
      (error "Loop stage ~a didn't have an init" name))
    (funcall init)))
