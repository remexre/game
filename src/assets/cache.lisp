(in-package :assets)

(defclass asset ()
  ((path :accessor asset-path :initarg :path :initform nil :type (or null pathname))))

(defun load-asset (path &key kind (renderer *renderer*))
  (cdr (load-asset-entry path :kind kind :renderer renderer)))

(defun load-asset-entry (path &key kind (renderer *renderer*))
  (unless (pathnamep path)
    (setf path (pathname path)))
  (setf path (truename path))

  (when-let (entry (assoc path (asset-cache renderer)))
    (return-from load-asset-entry entry))

  (let* ((asset (load-asset-uncached path :kind kind :renderer renderer))
         (entry (cons path asset)))
    (push entry (asset-cache renderer))
    entry))

(defun load-asset-uncached (path &key kind (renderer *renderer*))
  (unless kind
    (setf kind (eswitch ((pathname-type path) :test #'string=)
                 ("json" :prefab)
                 ("vx"   :model))))

  (let* ((asset (ecase kind
                  ((:model)
                   (load-model path :renderer renderer))
                  ((:prefab)
                   (load-prefab path :renderer renderer))))
         (entry (cons path asset)))
    (push entry (asset-cache renderer))
    entry))

(defun reload-asset (path &key kind (renderer *renderer*))
  (let ((asset (load-asset-uncached path :kind kind :renderer renderer))
        (entry (load-asset-entry    path :kind kind :renderer renderer)))
    (setf (cdr entry) asset)
    entry))
