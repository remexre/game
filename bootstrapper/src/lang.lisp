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

(set-macro 'defun
  (named-lambda defun (name args &body body)
    (list 'set-function (list 'quote name)
      (append (list 'named-lambda name args)
              body))))

(set-macro 'defmacro
  (named-lambda defmacro (name args &body body)
    (list 'set-macro (list 'quote name)
      (append (list 'named-lambda name args)
              body))))

(defun cons-arg-list (lst)
  (cond ((null lst)       nil)
        ((null (cdr lst)) (car lst))
        (t                (cons (car lst) (cons-arg-list (cdr lst))))))

(defun apply (func &rest args)
  (apply-1 func (cons-arg-list args)))

(defun print-id (x)
  (print x)
  x)

(defun symbol-function (sym)
  (get-function sym))

(defun assoc (item alist) ; TODO: key args
  (cond ((null alist) nil)
        ((eq (car (car alist)) item) (car alist))
        (t (assoc item (cdr alist)))))

(defun assoc-value (item alist)
  (cdr (assoc item alist)))

(defun not (x)
  (cond (x nil) (t t)))

(defmacro if (cond then else) 
  (list 'cond (list cond then) (list 't else)))

(defmacro unless (cond &body body)
  (list 'cond (cons (list 'not cond) body)))

(defmacro when (cond &body body)
  (list 'cond (cons cond body)))

(defun concat (lsts)
  (cond ((null lsts)       nil)
        ((null (cdr lsts)) (car lsts))
        (t (append (car lsts) (concat (cdr lsts))))))

(defun flat-map (func lst) ; TODO: More efficient implementation...
  (concat (map func lst)))

(defun last (lst)
  (if (null (cdr lst))
    (car lst)
    (last (cdr lst))))

(defun progn (&body body)
  (when body
    (last body)))

(defun map (func lst)
  (if (null lst)
    nil
    (cons (funcall func (car lst)) (map func (cdr lst)))))

(defun shl1 (sym form)
  (cond ((null form)               nil)
        ((null (cdr form))         nil)
        ((not (eq sym (car form))) nil)
        (t                         (null (cdr (cdr form))))))

(defun process-quasiquote (form)
  (cond ((atom form)                   (list 'list (list 'quote form)))
        ((shl1 'unquote form)          (list 'list (car (cdr form))))
        ((shl1 'unquote-splicing form) (car (cdr form)))
        (t                             (list 'list
                                         (cons 'append (map #'process-quasiquote form))))))

(defmacro quasiquote (form)
  (if (atom form)
    (list 'quote form)
    (cons 'append (map #'process-quasiquote form))))

(defmacro defpackage (name &rest properties)
  `(progn
     (define-package ,name)
     ,@(flat-map
         (lambda (pkg)
           (map
             (lambda (sym) `(import-to ,name (quote ,sym)))
             (exports-of pkg)))
         (assoc-value :use properties))))

(defmacro use-package (pkg)
  `(progn
     ,@(map (lambda (sym) `(import (quote ,sym))) (exports-of pkg))))

(defpackage :user
  (:use :lang))

(in-package 'user)
