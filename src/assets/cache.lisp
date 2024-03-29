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
       (setf entry (assoc path (renderer-asset-cache *renderer*) :test #'equal))
       (when entry
         (setf cachedp t)))

     ; If the entry wasn't present, load it.
     (unless entry
       (prn :assets "Loading ~a from ~a" kind path)
       (setf entry (cons path (call-next-method))))

     ; If the entry was uncached, but cache was requested, store it in the
     ; cache.
     (unless (or cachedp ignore-cache)
       (prn :assets "Caching ~a" path)
       (push entry (renderer-asset-cache *renderer*)))

     ; Return the asset or entry.
     (if get-entry
         entry
         (cdr entry)))))

(defgeneric asset-kind (asset)
  (:documentation "Contract: (asset-kind (load-asset kind path)) = kind"))

(defun reload-all-assets ()
  (iter
    (for entry in (reverse (renderer-asset-cache *renderer*)))
    (for path = (car entry))
    (for kind = (asset-kind (cdr entry)))
    (with-simple-restart (continue "Continue reloading assets")
      (setf (cdr entry) (load-asset kind path :ignore-cache t)))))
