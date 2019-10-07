(in-package :assets)

(defstruct script
  (pkg       (error "Must provide PKG")       :type package)
  (root-var  (error "Must provide ROOT-VAR")  :type symbol)
  (scene-var (error "Must provide SCENE-VAR") :type symbol))

(defmethod asset-kind ((script script))
  (declare (ignore script))
  :script)

(defmethod load-asset ((kind (eql :script)) path &key get-entry ignore-cache)
  (declare (ignore get-entry ignore-cache))

  ; DANGER: VERY CURSED
  (let* ((use-list  '(:alexandria :assets :cl :game-util :iterate :renderer :trivia))
         (pkg       (make-package (gensym) :use use-list))
         (root-var  (intern "*ROOT*" pkg))
         (scene-var (intern "*SCENE*" pkg)))
    (let ((*package* pkg))
      (eval `(defvar ,root-var))
      (eval `(defvar ,scene-var))
      (iter
        (for form in-file path)
        (eval form)))
    (let ((free (lambda () (delete-package pkg)))
          (script (make-script :pkg pkg :root-var root-var :scene-var scene-var)))
      (finalize script free)
      script)))

(defun script-on-event (script event &optional (scene (cdr (renderer-scene-entry *renderer*))))
  (when-let* ((sym (find-symbol "ON-EVENT" (script-pkg script)))
              (func (symbol-function sym)))
    ; N.B. Not setf!
    (set (script-root-var script) (scene-node scene))
    (set (script-scene-var script) scene)
    (funcall func event)))

(defun script-on-load (script &optional (scene (cdr (renderer-scene-entry *renderer*))))
  (when-let* ((sym (find-symbol "ON-LOAD" (script-pkg script)))
              (func (symbol-function sym)))
    ; N.B. Not setf!
    (set (script-root-var script) (scene-node scene))
    (set (script-scene-var script) scene)
    (funcall func)))
