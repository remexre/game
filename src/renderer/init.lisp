(in-package :renderer)

(defclass renderer ()
  ((window :initarg :window :reader window :type (satisfies cffi:pointerp))
   (program :accessor program :type fixnum)
   (scene :initform nil :accessor scene)
   (asset-cache :initform nil :accessor asset-cache :type list)))

(wadler-pprint:def-pretty-object renderer (:print-object t)
  (window scene asset-cache))

(defvar *renderer* 'not-initialized)

(defparameter +vert-shader-src+ (read-file #p"src/renderer/vert.glsl"))
(defparameter +frag-shader-src+ (read-file #p"src/renderer/frag.glsl"))

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

      (let ((vert (load-shader :vertex-shader   +vert-shader-src+))
            (frag (load-shader :fragment-shader +frag-shader-src+))
            (program (gl:create-program)))
        (gl:attach-shader program vert)
        (gl:attach-shader program frag)
        (gl:link-program program)
        (assert (gl:get-program program :link-status))
        (setf (program renderer) program))

      (setup-events window)
      (gl:bind-vertex-array (gl:gen-vertex-array))
      renderer)))

(defun load-shader (shader-type src)
  (let* ((shader (gl:create-shader shader-type)))
    (gl:shader-source shader src)
    (gl:compile-shader shader)
    (assert (gl:get-shader shader :compile-status))
    shader))

(defun flip (renderer)
  (check-type renderer renderer)
  (glfw:swap-buffers (window renderer)))

(defsetf title set-title)

(defun set-title (renderer title)
  (check-type renderer renderer)
  (check-type title string)
  (glfw:set-window-title title (window renderer)))
