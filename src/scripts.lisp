(in-package :game)

(def-loop-body :scripts (dt)
  (let ((scene (renderer-scene *renderer*)))
    (iter
      (for script-entry in (scene-script-entries scene))
      (for script = (cdr script-entry))
      (script/on-loop script dt))))
