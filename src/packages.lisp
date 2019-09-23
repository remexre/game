(defpackage :game-util
  (:use :alexandria :cl)
  (:export #:*log-tags* #:dbg #:prn))

(defpackage :renderer
  (:use :cffi :cl :game-util)
  (:export))

(defpackage :game
  (:use :alexandria :cl :game-util)
  (:export #:main))
