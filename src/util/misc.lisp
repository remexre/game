(in-package :game-util)

(defun assv (item alist)
  (cdr (assoc item alist)))

(defmacro bracket ((name start end) &body body)
  (check-type name symbol)
  `(let ((,name ,start))
     (unwind-protect (progn ,@body)
       ,end)))

(defun deg-to-rad (theta)
  (/ (* theta (coerce pi 'single-float)) 180.0))

(defun read-file (path)
  "Reads the file at PATH to a string."
  (with-open-file (stream path)
    (let ((out (make-string (file-length stream))))
      (read-sequence out stream)
      out)))

(defun read-json-file (path)
  "Reads the JSON file at PATH."
  (with-open-file (stream path)
    (cl-json:decode-json stream)))

(defun to-float-array (dims data)
  (make-array dims :element-type 'single-float :initial-contents data))
