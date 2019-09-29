(in-package :assets)

(defgeneric load-asset (kind path &key get-entry ignore-cache)
  (:method :around (kind path &key get-entry ignore-cache)
   (let (cachedp entry)
     ; Normalize the path.
     (unless (pathnamep path)
       (setf path (pathname path)))
     (setf path (truename path))

     ; Check the cache for the entry.
     (unless ignore-cache
       (setf entry (assoc path (asset-cache *renderer*))
             cachedp t))

     ; If the entry wasn't present, load it.
     (unless entry
       (setf entry (call-next-method)))

     ; If the entry was uncached and the cache wasn't explicitly ignored, store
     ; it in the cache.
     (unless (or cachedp ignore-cache)
       (push entry (asset-cache *renderer*)))

     ; Return the asset or entry.
     (if get-entry
         entry
         (cdr entry)))))

(defun load-asset (path &key kind)
  (cdr (load-asset-entry path :kind kind)))

(defun load-asset-entry (path &key kind)
  (setf path (truename path))


  (let ((entry (load-asset-entry-uncached path :kind kind)))
    (push entry (asset-cache *renderer*))
    entry))

(defun load-asset-entry-uncached (path &key kind)
  (unless kind
    (setf kind (eswitch ((pathname-type path) :test #'string=)
                        ("json" :prefab)
                        ("vx"   :model))))

  (let* ((asset (ecase kind
                  ((:model)
                   (load-model path))
                  ((:prefab)
                   (load-prefab path))))
         (entry (cons path asset)))
    (push entry (asset-cache *renderer*))
    entry))

(defun reload-asset (path &key kind)
  (let ((asset (cdr (load-asset-entry-uncached path :kind kind)))
        (entry      (load-asset-entry          path :kind kind)))
    (setf (cdr entry) asset)
    entry))
