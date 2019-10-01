(in-package :game)

(def-loop-body :drain-events ()
  (iter
    (for event in *events*)
    (prn :events "~s" event))
  (setf *events* nil))

(opts:define-opts
  (:name :debug
   :description "Enable debugging stage"
   :short #\d
   :long "debug"))

(defun main ()
  (multiple-value-bind (opts args) (opts:get-opts)
    (declare (ignore args))
    (when (getf opts :debug)
      (enable-loop-stage :debug))

    (enable-loop-stages :events :drain-events :renderer :fps)
    (setf (renderer-scene-entry *renderer*)
          (load-asset :scene #p"assets/scenes/triangle.json" :get-entry t))
    (main-loop)))
