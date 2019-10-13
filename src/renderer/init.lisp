(in-package :renderer)

(defstruct renderer
  (window (error "Must provide WINDOW") :type (satisfies cffi:pointerp))
  (program 0 :type fixnum)
  (scene-entry nil :type (or (cons pathname assets:scene) null))
  (asset-cache nil :type list)
  (title-fields nil :type list))

(defun renderer-scene (renderer)
  (check-type renderer renderer)
  (cdr (renderer-scene-entry renderer)))

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
  (prn :debug "Reloading program...")
  (setf +vert-shader-src+ (read-file #p"src/renderer/vert.glsl")
        +frag-shader-src+ (read-file #p"src/renderer/frag.glsl"))
  (setf (renderer-program *renderer*) (load-program))
  (gl:use-program (renderer-program *renderer*)))

(defun flip (renderer)
  (check-type renderer renderer)
  (glfw:swap-buffers (renderer-window renderer))
  (let* ((fields (renderer-title-fields renderer))
         (parts  (mapcar (lambda (p) (format nil "~a: ~a" (car p) (cdr p))) fields))
         (title  (format nil "~{~a~^, ~}" parts)))
    (glfw:set-window-title title (renderer-window renderer))))

(defsetf renderer-title-field set-renderer-title-field)

(defun set-renderer-title-field (renderer field value)
  (check-type renderer renderer)
  (check-type field string)
  (let* ((fields (renderer-title-fields renderer))
         (entry  (assoc field fields :test #'string=)))
    (cond
      (entry  (setf (cdr entry) value))
      (fields (push (cons field value) (cdr (last fields))))
      (t      (setf (renderer-title-fields renderer) (list (cons field value)))))))
