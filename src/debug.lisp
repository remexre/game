(in-package :game)

(defconstant +sigusr1+ 10)
(defparameter *should-reload* nil)

(cffi:defcallback reload-signal-handler :void ((signo :int))
  (setf *should-reload* t))

(def-loop-init :debug ()
  (setf swank:*communication-style* nil)
  (with-simple-restart (abort "Don't try to start SWANK.")
    (swank:create-server :style :fd-handler :dont-close t))
  (cffi:foreign-funcall "signal" :int +sigusr1+ :pointer (cffi:callback reload-signal-handler)))

(def-loop-body :debug ()
  (sb-impl::serve-event 0)
  (iter
    (for conn in swank::*connections*)
    (swank::handle-requests conn t))
  (when *should-reload*
    (setf *should-reload* nil)
    (prn :reload "Reloading...")
    (asdf:load-system :game)
    (cffi:load-foreign-library 'renderer::renderer)))
