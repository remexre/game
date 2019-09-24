(in-package :renderer)

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

(defclass renderer ()
  ((pointer :initarg :pointer :reader renderer-pointer)))

(defun make-renderer ()
  (let* ((pointer (renderer-init))
         (free (lambda () (renderer-exit pointer)))
         (renderer (make-instance 'renderer :pointer pointer)))
    (finalize renderer free)
    renderer))
