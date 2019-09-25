(in-package :renderer)

(defcfun "renderer_poll_events" :uint64
  (state renderer-state))

(defcfun "renderer_get_events" :void
  (state renderer-state)
  (data  (:pointer :float))
  (len   :uint64))

(defun get-events (renderer)
  (check-type renderer renderer)

  (let* ((len (renderer-poll-events (pointer renderer)))
         (str (with-foreign-pointer (buf len)
                (renderer-get-events (pointer renderer) buf len)
                (foreign-string-to-lisp buf))))
    (with-input-from-string (stream str)
      (read stream))))
