(defpackage :game-util
  (:use :alexandria :cl :iterate)
  (:export #:assv #:bracket #:read-file #:read-json-file #:to-float-array
           #:*log-tags* #:dbg #:prn
           #:xform #:+identity-xform+ #:apply-xform #:compose-xforms #:xform-composef
           #:xform-rot #:xform-scale #:xform-xlat))

(defpackage :renderer
  (:use :cl :game-util :iterate :trivial-garbage :trivial-main-thread)
  (:export #:*renderer*
           #:asset-cache #:flip #:make-renderer #:renderer #:scene #:title
           #:immutable-buffer #:make-immutable-buffer
           #:get-events))

(defpackage :assets
  (:use :alexandria :cl :game-util :iterate :renderer :trivia :trivial-shell)
  (:export #:load-asset #:reload-all-assets

           )
  (:import-from :wadler-pprint #:def-pretty-object))

(defpackage :game
  (:use :alexandria :assets :cl :game-util :iterate :renderer :trivia)
  (:export #:main))
