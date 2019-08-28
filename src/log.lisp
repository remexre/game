(in-package #:game)

(defvar *csi* (coerce (list (code-char #x1b) #\[) 'string))
(defvar *stderr* *error-output*)
(defvar *stderr-isatty* (interactive-stream-p *stderr*))
(defvar *log-program-start-time* (get-universal-time))

(defvar *log-caller* t)
(defvar *log-targets* nil)

(defun time-since-start ()
  (- (get-universal-time) *log-program-start-time*))

(defun log-prim (target str &key (color 4) (frames 2))
  (check-type target (or keyword (eql t)))
  (check-type str string)

  (when (or (eq target t) (member target *log-targets*))
    (let ((color-start "") (color-end "") (caller ""))
      (when (and *stderr-isatty* (eq *error-output* *stderr*))
        (setf color-start (format nil "~a3~am" *csi* color))
        (setf color-end (concatenate 'string *csi* "0m")))
      (when *log-caller*
        (let ((stack (dissect:stack)))
          (when-let (frame (nth frames stack))
                    (setf caller (format nil "[~a]" (dissect:call frame))))))
      (format *error-output* "~a[~,3f]~a~a ~a~%" color-start (time-since-start)
              caller color-end str))))

(defun lg (target fmt &rest args)
  "Logs a message."
  (log-prim target (apply #'format nil fmt args)))

(defun err (target fmt &rest args)
  "Logs an error message."
  (log-prim target (apply #'format nil fmt args) :color 1))

(defun die (target fmt &rest args)
  "Logs an error message and exits."
  (log-prim target (apply #'format nil fmt args) :color 1)
  (exit 1))

(define-condition todo ()
  ())

(defun todo ()
  (let ((*log-caller* t))
    (log-prim t "TODO" :color 1))
  (error 'todo))
