(in-package :renderer)

(defclass renderer ()
  ((window :initarg :window :reader window :type (satisfies cffi:pointerp))
   (scene  :initform nil :accessor scene)
   (asset-cache :initform nil :accessor asset-cache :type list)))

(wadler-pprint:def-pretty-object renderer (:print-object t)
  (window scene asset-cache))

(defvar *renderer* 'not-initialized)

(defun make-renderer ()
  (with-body-in-main-thread ()
    ; glfw is resistant to non-global state, not wholly unreasonably. So we
    ; have to word this a bit awkwardly...
    (glfw:set-error-callback 'glfw::default-error-fun)
    (glfw:initialize)
    (glfw:create-window :width 800 :height 600 :title "game"
                        :context-version-major 3
                        :context-version-minor 3
                        :opengl-profile :opengl-core-profile)
    (let* ((window glfw:*window*)
           (free (lambda ()
                   (glfw:destroy-window window)))
           (renderer (make-instance 'renderer :window window)))
      (finalize renderer free)
      (setup-events window)
      (gl:bind-vertex-array (gl:gen-vertex-array))
      renderer)))

(defun flip (renderer)
  (check-type renderer renderer)
  (glfw:swap-buffers (window renderer)))

(defsetf title set-title)

(defun set-title (renderer title)
  (check-type renderer renderer)
  (check-type title string)
  (glfw:set-window-title title (window renderer)))
