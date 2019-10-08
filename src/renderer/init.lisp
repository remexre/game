(in-package :renderer)

(defstruct renderer
  (window (error "Must provide WINDOW") :type (satisfies cffi:pointerp))
  (program 0 :type fixnum)
  (scene-entry nil :type (or (cons pathname assets:scene) null))
  (asset-cache nil :type list))

(defvar *renderer* 'not-initialized)

(defparameter +vert-shader-src+ (read-file #p"src/renderer/vert.glsl"))
(defparameter +frag-shader-src+ (read-file #p"src/renderer/frag.glsl"))

(defun init-renderer ()
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
           (renderer (make-renderer :window window)))
      (finalize renderer free)

      ; We do this after the finalize call so if it fails, GLFW may still be
      ; terminated.
      (setf (renderer-program renderer) (load-program))

      (setup-events window)
      (gl:bind-vertex-array (gl:gen-vertex-array))

  ; Set things we ought to be able to lift out...
  ; (gl:enable :cull-face)
  (gl:enable :depth-test)
  (gl:use-program (renderer-program renderer))
  (gl:enable-vertex-attrib-array 0)
  (gl:enable-vertex-attrib-array 1)
  (gl:enable-vertex-attrib-array 2)
      renderer)))

(defun load-shader (shader-type src)
  (let* ((shader (gl:create-shader shader-type)))
    (gl:shader-source shader src)
    (gl:compile-shader shader)
    (unless (gl:get-shader shader :compile-status)
      (error "Failed to compile shader: ~a" (gl:get-shader-info-log shader)))
    shader))

(defun load-program ()
  (let ((vert (load-shader :vertex-shader   +vert-shader-src+))
        (frag (load-shader :fragment-shader +frag-shader-src+))
        (program (gl:create-program)))
    (gl:attach-shader program vert)
    (gl:attach-shader program frag)
    (gl:link-program program)
    (unless (gl:get-program program :link-status)
      (error "Failed to link program: ~a" (gl:get-program-info-log program)))
    program))

(defun reload-program ()
  (setf +vert-shader-src+ (read-file #p"src/renderer/vert.glsl")
        +frag-shader-src+ (read-file #p"src/renderer/frag.glsl"))
  (setf (renderer-program *renderer*) (load-program)))

(defun flip (renderer)
  (check-type renderer renderer)
  (glfw:swap-buffers (renderer-window renderer)))

(defsetf title set-title)

(defun set-title (renderer title)
  (check-type renderer renderer)
  (check-type title string)
  (glfw:set-window-title title (renderer-window renderer)))
