(in-package #:game)

(opts:define-opts
  (:name        :help
   :description "print this help text"
   :short       #\h
   :long        "help")
  (:name        :swank
   :description "start swank"
   :long        "swank"))

(defun cli-main ()
  (multiple-value-bind (options args) (opts:get-opts)
    (when (getf options :help)
      (opts:describe
        :prefix "QWERTY"
        )
      (opts:exit))
    (main args (getf options :output-path))))

(defun main (src-paths output-path)
  (declare (ignore output-path))
  (let* ((parsed-modules (mapcar #'parse-file src-paths))
         (loaded-modules nil))
    (format t "parsed-modules = ~a~%" parsed-modules)
    (dolist (module (toposort parsed-modules #'module-depends-on))
      (resolve-names-for-module module loaded-modules)
      (push (cons (name module) module) loaded-modules))))
