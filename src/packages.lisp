(defpackage :game-util
  (:use :alexandria :cl)
  (:export #:assv
           #:*log-tags* #:dbg #:prn))

(defpackage :renderer
  (:use :cl :game-util :iterate :trivial-garbage :trivial-main-thread)
  (:export #:*renderer*
           #:asset-cache #:flip #:make-renderer #:renderer #:scene #:title
           #:immutable-buffer #:make-immutable-buffer
           #:get-events))

(defpackage :assets
  (:use :alexandria :cl :game-util :renderer :trivia :trivial-shell)
  (:export #:load-asset #:asset-path #:asset-renderer
           #:load-model #:model
           #:load-prefab #:prefab #:prefab-tree

           #:render-tree
           #:render-clear-color #:render-clear-color-r #:render-clear-color-g
           #:render-clear-color-b #:render-clear-color-a
           #:render-group #:render-group-children
           #:render-model #:render-prefab #:render-entry)
  (:import-from :wadler-pprint #:def-pretty-object))

(defpackage :game
  (:use :alexandria :assets :cl :game-util :iterate :renderer :trivia)
  (:export #:main))
