(defpackage :game-util
  (:use :alexandria :cl :cl-fsnotify)
  (:export
    #:*log-tags* #:dbg #:prn
    #:reload))

(defpackage :renderer
  (:use :cffi :cl :game-util :trivial-garbage)
  (:export #:clear-color #:flip #:make-renderer #:renderer #:title))

(defpackage :game
  (:use :alexandria :cl :game-util :iterate :renderer)
  (:export #:main))
