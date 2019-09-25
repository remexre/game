(defpackage :game-util
  (:use :alexandria :cl :cl-fsnotify)
  (:export
    #:*log-tags* #:dbg #:prn
    #:reload))

(defpackage :renderer
  (:use :cffi :cl :game-util :trivial-garbage)
  (:export #:clear-color #:flip #:make-renderer #:renderer #:title
           #:immutable-buffer #:make-immutable-buffer
           #:get-events))

(defpackage :assets
  (:use :cl :game-util :renderer)
  (:export #:load-model #:model))

(defpackage :game
  (:use :alexandria :cl :game-util :iterate :renderer)
  (:export #:main))
