(in-package :assets)

(defstruct script
  (pkg (error "Must provide PKG") :type package))

(defmethod asset-kind ((script script))
  (declare (ignore script))
  :script)

(defmethod load-asset ((kind (eql :script)) path &key get-entry ignore-cache)
  (declare (ignore get-entry ignore-cache))

  ; DANGER: VERY CURSED
  (let ((pkg (make-package (gensym) :use '(:alexandria :cl :game-util :iterate :renderer :trivia))))
    (let ((*package* pkg))
      (iter
        (for form in-file path)
        (eval form)))
    (delete-package pkg)
    (make-script :pkg pkg)))

(defun script-onevent (script scene event)
  ;
  )

(defun script-onload (script scene)
  ;
  )
