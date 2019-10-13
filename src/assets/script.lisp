(in-package :assets)

(defstruct script
  (pkg       (error "Must provide PKG")       :type package)
  (root-var  (error "Must provide ROOT-VAR")  :type symbol))

(defmethod asset-kind ((script script))
  (declare (ignore script))
  :script)

(defmethod load-asset ((kind (eql :script)) path &key get-entry ignore-cache)
  (declare (ignore get-entry ignore-cache))

  ; DANGER: VERY CURSED
  (let* ((use-list  '(:alexandria :assets :cl :game-util :iterate :renderer :trivia))
         (pkg       (make-package (gensym) :use use-list))
         (root-var  (intern "*ROOT*" pkg)))
    (let ((*package* pkg))
      (eval `(defvar ,root-var))
      (iter
        (for form in-file path)
        (eval form)))
    (let ((free (lambda () (delete-package pkg)))
          (script (make-script :pkg pkg :root-var root-var)))
      (finalize script free)
      script)))

(defvar *scene* nil)

(defun script-call (script sym &rest args)
  (when-let* ((sym (find-symbol sym (script-pkg script)))
              (func (symbol-function sym)))
    (let ((*scene* (or *scene* (renderer-scene *renderer*))))
      ; N.B. Not setf!
      (set (script-root-var script) (scene-node *scene*))
      (apply func args))))

(defun script/on-event (script event &optional *scene*)
  (script-call script "ON-EVENT" event))

(defun script/on-load (script &optional *scene*)
  (script-call script "ON-LOAD"))

(defun script/on-loop (script dt &optional *scene*)
  (script-call script "ON-LOOP" dt))
