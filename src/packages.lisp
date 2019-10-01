(defpackage :game-util
  (:use :alexandria :cl :iterate)
  (:export #:assv #:bracket #:read-file #:read-json-file #:to-float-array
           #:*log-tags* #:dbg #:prn
           #:xform #:+identity-xform+ #:apply-xform #:compose-xforms
           #:xform-composef #:flatten-xform
           #:xform-rot #:xform-scale #:xform-xlat))

(defpackage :renderer
  (:use :cl :game-util :iterate :trivial-garbage :trivial-main-thread)
  (:export #:*renderer*
           #:flip #:init-renderer #:renderer #:renderer-asset-cache
           #:renderer-scene-entry #:title
           #:immutable-buffer #:make-immutable-buffer
           #:*shader-proj* #:*shader-view* #:*shader-model* #:*shader-diffuse*
           #:*drawn-triangles* #:clear #:draw-object
           #:get-events))

(defpackage :assets
  (:use :alexandria :cl :game-util :iterate :parse-float :renderer :trivia
        :trivial-shell)
  (:export #:load-asset #:reload-all-assets
           #:camera #:camera-pos #:camera-up #:camera-front #:camera-near
                    #:camera-far #:camera-fov #:camera-ortho
                    #:camera-proj-xform #:camera-view-xform
           #:model #:model-buf
           #:node
           #:node-include-prefab #:node-include-prefab-entry
           #:node-lod-branch #:node-lod-branch-distance #:node-lod-branch-closer
                             #:node-lod-branch-further
           #:node-model #:node-model-entry
           #:node-shader-params #:node-shader-params-diffuse
                                #:node-shader-params-child
           #:node-xform #:node-xform-matrix #:node-xform-child
           #:scene #:scene-camera #:scene-clear-color #:scene-children))

(defpackage :game
  (:use :alexandria :assets :cl :game-util :iterate :renderer :trivia)
  (:export #:main))
