(asdf:defsystem #:game
  :description "A game."
  :author "Nathan Ringo <nathan@remexre.xyz>"
  :license  "Apache-2.0/MIT"
  :version "0.0.1"
  :serial t
  :build-operation "program-op"
  :build-pathname "out/game"
  :entry-point "game:main"
  :depends-on (:alexandria :bordeaux-threads :cl-glfw3 :cl-opengl :iterate :swank :trivial-garbage
               :trivial-main-thread)
  :components ((:file "src/package")

               (:file "src/utils")
               (:file "src/loop")
               (:file "src/swank-module")
               (:file "src/thunks")

			   (:file "src/ogl-program")
			   (:file "src/ogl-vabo")
			   (:file "src/assets")

               (:file "src/render-fps")
               (:file "src/render-math")
               (:file "src/render")

               (:file "src/main")))

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))

; vi: ft=lisp :
