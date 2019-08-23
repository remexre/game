(defpackage #:game
  (:use #:cl #:cl-glfw3 #:cl-opengl #:iterate #:trivial-main-thread)
  (:export #:main #:swank)
  (:shadowing-import-from :cl-opengl :finish)
  (:shadowing-import-from :cl-glfw3 :terminate))
