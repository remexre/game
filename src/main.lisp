(set-macro 'defmacro
  (named-lambda defmacro (name args &body foo)
    (cons 'print (cons ''todo-defmacro (cons (cons 'quote (cons name nil)) nil)))))

(defmacro let* (clauses &body foo)
  '(print 'todo))

(in-package 'user)
