(defsystem #:game
  :author "Nathan Ringo <nathan@remexre.xyz>"
  :license "Apache-2.0/MIT"
  :serial t
  :depends-on (:alexandria :cffi :iterate :swank :trivial-garbage)
  :components ((:file "src/packages")

               (:file "src/renderer")

               (:file "src/main")))

; vi: ft=lisp :
