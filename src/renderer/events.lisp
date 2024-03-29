(in-package :renderer)

(defvar *events* nil)

(glfw:def-key-callback key-callback (window key scancode action mod-keys)
  (declare (ignore scancode))
  (with-body-in-main-thread ()
    (push (list window :keyboard key action mod-keys) *events*)))

(glfw:def-window-size-callback window-size-callback (window w h)
  (with-body-in-main-thread ()
    (push (list window :resize w h) *events*)))

(defun setup-events (window)
  (glfw:set-key-callback 'key-callback window)
  (glfw:set-window-size-callback 'window-size-callback window))

(defun get-events ()
  (glfw:poll-events)
  (let* ((out nil)
         (func (lambda (event)
                 (and (eql (cffi:pointer-address (car event))
                           (cffi:pointer-address (renderer-window *renderer*)))
                      (push (cdr event) out)))))
    (setf *events* (delete-if func *events*))
    (when (glfw:window-should-close-p (renderer-window *renderer*))
      (push :close-requested out))
    out))
