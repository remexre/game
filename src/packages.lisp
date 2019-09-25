(defpackage :game-util
  (:use :alexandria :cl :cl-fsnotify)
  (:export
    #:*log-tags* #:dbg #:prn
    #:reload))

(defpackage :renderer
  (:use :cffi :cl :game-util :trivial-garbage)
  (:export #:flip #:make-renderer #:renderer))

(defpackage :game
  (:use :alexandria :cl :game-util :iterate)
  (:export #:main))
