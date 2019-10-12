(defsystem "game"
  :author "Nathan Ringo <nathan@remexre.xyz>"
  :license "Apache-2.0/MIT"
  :serial t
  :build-operation "program-op"
  :build-pathname "game"
  :entry-point "game:main"
  :depends-on ("alexandria" "cl-glfw3" "cl-json" "cl-opengl" "cl-wadler-pprint"
               "ieee-floats" "iterate" "parse-float" "png-read" "swank"
               "swap-bytes" "trivia" "trivial-garbage" "trivial-main-thread"
               "trivial-shell" "unix-opts")
  :components ((:file "src/packages")

               (:file "src/util/misc")
               (:file "src/util/log")
               (:file "src/util/vec3")
               (:file "src/util/xform")

               (:static-file "src/renderer/frag.glsl")
               (:static-file "src/renderer/vert.glsl")
               (:file "src/renderer/init")
               (:file "src/renderer/bufs")
               (:file "src/renderer/imgs")
               (:file "src/renderer/events")
               (:file "src/renderer/draw")

               (:file "src/assets/cache")
               (:file "src/assets/camera")
               (:file "src/assets/model")
               (:file "src/assets/texture")
               (:file "src/assets/node")
               (:file "src/assets/prefab")
               (:file "src/assets/scene")
               (:file "src/assets/script")

               (:file "src/loop")
               (:file "src/debug")
               (:file "src/renderer")
               (:file "src/events")
               (:file "src/fps")
               (:file "src/main")))

; vi: ft=lisp :
