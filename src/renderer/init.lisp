(in-package :renderer)

(defclass renderer ()
  ((pointer :initarg :pointer :reader pointer)
   (clear-color :initform #(0.0 0.0 0.0 0.0) :accessor clear-color)))

(define-foreign-library renderer
  (t (:or "./librenderer.so"
          "./target/release/librenderer.so"
          "./target/debug/librenderer.so"
          "librenderer")))

(use-foreign-library renderer)

(defctype renderer-state :pointer)

(defcfun "renderer_init" renderer-state)
(defcfun "renderer_exit" :void
  (state renderer-state))

(defun make-renderer ()
  (let* ((pointer (renderer-init))
         (free (lambda () (renderer-exit pointer)))
         (renderer (make-instance 'renderer :pointer pointer)))
    (finalize renderer free)
    renderer))

(defcfun "renderer_draw" :void
  (state renderer-state)
  (r :float)
  (g :float)
  (b :float)
  (a :float))

(defun flip (renderer)
  (check-type renderer renderer)
  (let* ((cc (clear-color renderer))
         (r (coerce (aref cc 0) 'single-float))
         (g (coerce (aref cc 1) 'single-float))
         (b (coerce (aref cc 2) 'single-float))
         (a (coerce (aref cc 3) 'single-float)))
    (renderer-draw (pointer renderer) r g b a)))

(defcfun "renderer_set_title" :void
  (state renderer-state)
  (ptr   :string))

(defsetf title set-title)

(defun set-title (renderer title)
  (check-type renderer renderer)
  (check-type title string)
  (with-foreign-string (ptr title)
    (renderer-set-title (pointer renderer) ptr)))
