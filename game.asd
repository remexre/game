(defsystem #:game
  :author "Nathan Ringo <nathan@remexre.xyz>"
  :license "Apache-2.0/MIT"
  :serial t
  :depends-on (:alexandria :cffi :cl-fsnotify :iterate :swank :trivial-garbage)
  :components ((:file "src/packages")

               (:file "src/util/log")

               (:file "src/renderer/init")
               (:file "src/renderer/flip")

               (:file "src/loop")
               (:file "src/debug")
               (:file "src/renderer")
               (:file "src/main")))

; vi: ft=lisp :
