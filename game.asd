(defsystem #:game
  :author "Nathan Ringo <nathan@remexre.xyz>"
  :license "Apache-2.0/MIT"
  :serial t
  :build-operation "program-op"
  :build-pathname "game"
  :entry-point "game:main"
  :depends-on (:alexandria :cl-glfw3 :cl-json :cl-opengl :cl-wadler-pprint
               :iterate :swank :trivia :trivial-garbage :trivial-main-thread
               :trivial-shell)
  :components ((:file "src/packages")

               (:file "src/util/misc")
               (:file "src/util/log")

               (:file "src/renderer/init")
               (:file "src/renderer/bufs")
               (:file "src/renderer/events")

               (:file "src/assets/cache")
               (:file "src/assets/model")
               (:file "src/assets/tree")
               (:file "src/assets/prefab")

               (:file "src/loop")
               (:file "src/debug")
               (:file "src/xform")
               (:file "src/renderer")
               (:file "src/events")
               (:file "src/fps")
               (:file "src/main")))

; vi: ft=lisp :
