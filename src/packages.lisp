(defpackage :game-util
  (:use :alexandria :cl :cl-fsnotify)
  (:export
    #:*log-tags* #:dbg #:prn
    #:reload #:reload-loop))

(defpackage :renderer
  (:use :cffi :cl :game-util :trivial-garbage)
  (:export #:make-renderer))

(defpackage :game
  (:use :alexandria :cl :game-util)
  (:export #:main))
