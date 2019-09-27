(defsystem #:game
  :author "Nathan Ringo <nathan@remexre.xyz>"
  :license "Apache-2.0/MIT"
  :serial t
  :build-operation "program-op"
  :build-pathname "target/game"
  :entry-point "game:main"
  :depends-on (:alexandria :cffi :cl-json :iterate :swank :trivia :trivial-garbage :trivial-shell)
  :components ((:file "src/packages")

               (:file "src/util/misc")
               (:file "src/util/log")

               (:file "src/renderer/init")
               (:file "src/renderer/bufs")
               (:file "src/renderer/draw")
               (:file "src/renderer/events")

               (:file "src/assets/model")
               (:file "src/assets/prefab")

               (:file "src/loop")
               (:file "src/debug")
               (:file "src/renderer")
               (:file "src/events")
               (:file "src/fps")
               (:file "src/main")))

; vi: ft=lisp :
