(defpackage :game-util
  (:use :alexandria :cl)
  (:export #:assv
           #:*log-tags* #:dbg #:prn))

(defpackage :renderer
  (:use :cffi :cl :game-util :iterate :trivial-garbage)
  (:export #:*renderer*
           #:asset-cache #:clear-color #:flip #:make-renderer #:renderer #:title
           #:immutable-buffer #:make-immutable-buffer
           #:get-events))

(defpackage :assets
  (:use :alexandria :cl :game-util :renderer :trivia :trivial-shell)
  (:export #:load-asset #:asset-path #:asset-renderer
           #:load-model #:model
           #:load-prefab #:prefab))

(defpackage :game
  (:use :alexandria :assets :cl :game-util :iterate :renderer :trivia)
  (:export #:main))
