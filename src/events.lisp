(in-package :game)

(defvar *events* nil)

(def-loop-body :events ()
  (dolist (event (get-events *renderer*))
    (match event
      (:close-requested (setf *continue-loop* nil))
      ((list :keyboard scancode state mods)
       (when-let (name (translate-scancode scancode))
         (push (list :keyboard name state mods) *events*)))
      (_ (prn t "unknown event ~s" event))))

  (when (> (length *events*) 10)
    (prn :events "~a events queued!" (length *events*))))

(defun translate-scancode (scancode)
  (case scancode
    (1   :escape)
    (2   #\1)
    (3   #\2)
    (4   #\3)
    (5   #\4)
    (6   #\5)
    (7   #\6)
    (8   #\7)
    (9   #\8)
    (10  #\9)
    (11  #\0)
    (14  :backspace)
    (16  #\q)
    (57  :space)
    (103 :up)
    (105 :left)
    (106 :right)
    (108 :down)
    (t (prn :events "unknown scancode: ~s" scancode))))
