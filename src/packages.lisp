(defpackage :game-util
  (:use :alexandria :cl :iterate)
  (:export #:assv #:bracket #:deg-to-rad #:read-file #:read-json-file #:to-float-array
           #:*log-tags* #:dbg #:prn
           #:vec4-to-vec3 #:vec3-add #:vec3-cross #:vec3-float-mul
                          #:vec3-magnitude #:vec3-normalize #:vec3-sub
           #:xform #:+identity-xform+ #:apply-xform #:apply-xform-neg-unit-w
                   #:apply-xform-unit-w #:compose-xforms #:xform-composef
                   #:flatten-xform #:xform-rot-x #:xform-rot-y #:xform-rot-z
                   #:xform-scale #:xform-xlat))

(defpackage :renderer
  (:use :cl :game-util :iterate :trivial-garbage :trivial-main-thread)
  (:export #:*renderer*
           #:flip #:init-renderer #:reload-program
           #:renderer #:renderer-asset-cache #:renderer-camera
                      #:renderer-scene-entry #:title
           #:immutable-buffer #:make-immutable-buffer
           #:immutable-image #:make-immutable-image
                             #:make-immutable-image-from-png
           #:*shader-light-position*
           #:*shader-proj-xform* #:*shader-view-xform* #:*shader-model-xform*
           #:*shader-ambient* #:*shader-diffuse*
           #:*drawn-triangles* #:clear #:draw-object
           #:get-events))

(defpackage :assets
  (:use :alexandria :cl :game-util :iterate :parse-float :renderer :trivia
        :trivial-garbage :trivial-shell)
  (:export #:load-asset #:reload-all-assets
           #:camera #:camera-pos #:camera-up #:camera-rot #:camera-near
                    #:camera-far #:camera-fov #:camera-aspect-ratio
                    #:camera-ortho #:camera-front #:camera-right
                    #:camera-proj-xform #:camera-view-xform
           #:camera-move #:camera-rotate
           #:model #:model-buf
           #:texture #:texture-img
           #:font #:font-texture
           #:node #:parse-node
           #:node-include-prefab #:node-include-prefab-entry
           #:node-lod-branch #:node-lod-branch-distance
                             #:node-lod-branch-closer
                             #:node-lod-branch-further
           #:node-model #:node-model-entry
           #:node-multi #:node-multi-children
           #:node-shader-params #:node-shader-params-ambient
                                #:node-shader-params-child
                                #:node-shader-params-diffuse
           #:node-xform #:node-xform-matrix #:node-xform-child
           #:prefab #:prefab-node
           #:scene #:scene-camera #:scene-clear-color #:scene-node
                   #:scene-script-entries
           #:script #:script-on-event #:script-on-load))

(defpackage :game
  (:use :alexandria :assets :cl :game-util :iterate :renderer :trivia)
  (:export #:main))
