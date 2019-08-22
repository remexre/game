(asdf:defsystem #:game
  :description "A game."
  :author "Nathan Ringo <nathan@remexre.xyz>"
  :license  "Apache-2.0/MIT"
  :version "0.0.1"
  :serial t
  :build-operation "program-op"
  :build-pathname "out/game"
  :entry-point "game:main"
  :depends-on (:cl-glfw3 :cl-opengl :trivial-main-thread)
  :components ((:file "src/package")
               (:file "src/utils")
               (:file "src/main")))
