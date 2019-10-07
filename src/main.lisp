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
   :long "debug") 
  (:name :help
   :description "Display this help message"
   :short #\h
   :long "help"))

(defun usage ()
  (opts:describe
    :usage-of "./game"
    :args "scene.json")
  (opts:exit 1))

(defun main ()
  (multiple-value-bind (opts args) (opts:get-opts)
    (when (getf opts :debug)
      (enable-loop-stage :debug))
    (when (getf opts :help)
      (usage))

    (unless (eql (length args) 1)
      (usage))

    (let ((scene-path (pathname (nth 0 args))))
      (enable-loop-stages :events :drain-events :renderer :fps)
      (setf (renderer-scene-entry *renderer*) (load-asset :scene scene-path :get-entry t))
      (main-loop))))
