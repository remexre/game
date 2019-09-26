(defpackage :game-util
  (:use :alexandria :cl)
  (:export
    #:*log-tags* #:dbg #:prn
    #:reload))

(defpackage :renderer
  (:use :cffi :cl :game-util :iterate :trivial-garbage)
  (:export #:clear-color #:flip #:make-renderer #:renderer #:title
           #:immutable-buffer #:make-immutable-buffer
           #:get-events))

(defpackage :assets
  (:use :cl :game-util :renderer :trivial-shell)
  (:export #:load-model #:model))

(defpackage :game
  (:use :alexandria :assets :cl :game-util :iterate :renderer :trivia)
  (:export #:main))
