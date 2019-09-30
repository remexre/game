(in-package :game-util)

(defvar *log-tags* t)

(defun log-tag-enabled (tag &optional (tags *log-tags*))
  "Returns whether tag is T, tags is termined by T, or tag is a member of tags."
  (when (eq tag t)
    (return-from log-tag-enabled t))
  (loop
    while (consp tags)
    if (eq tag (car tags))
      do (return-from log-tag-enabled t)
    else
      do (setf tags (cdr tags)))
  (eq tags t))

(defun prn (tag format-control &rest args)
  (unless (log-tag-enabled tag)
    (return-from prn))
  (format *error-output* "~&~?~%" format-control args))

(defmacro dbg (val)
  (with-gensyms (name)
    `(let ((,name ,val))
       (prn :dbg ,(format nil "~s = (the ~~s ~~s)" val) (type-of ,name) ,name)
       ,name)))
