(set-function 'list (named-lambda list (&rest args) args))

(set-function 'append-2
  (named-lambda append-2 (l r)
    (cond ((atom l) r)
          (t (cons (car l) (append-2 (cdr l) r))))))

(set-function 'append
  (named-lambda append (&rest args)
    (cond ((null args)       nil)
          ((null (cdr args)) (car args))
          (t                 (append-2 (car args)
                                       (apply-1 (get-function 'append) (cdr args)))))))

(set-function 'cons-arg-list
  (named-lambda cons-arg-list (lst)
    (cond ((null lst)       nil)
          ((null (cdr lst)) (car lst))
          (t                (cons (car lst) (cons-arg-list (cdr lst)))))))

(set-function 'apply
  (named-lambda apply (func &rest args)
    (apply-1 func (cons-arg-list args))))

; (in-package 'user)
