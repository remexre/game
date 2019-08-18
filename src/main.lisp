(set-symbol-function 'hello (lambda () (print 'hello)))
(hello)

(funcall
  (lambda (x)
   (funcall
     (lambda (x) (funcall x 1))
     (lambda (y) (funcall x 2))))
  (lambda (z) 42))

(hello)
