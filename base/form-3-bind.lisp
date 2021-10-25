;; -*- coding: utf-8 -*-
;; This file is part of CL-JKit.
;; Copyright (c) 2012-2021 tradcrafts

(jkit.core:jkit-core-header)

(in-package :jkit.base.form)

(defun <bind-make-defs> (params body)
  (bind (((bind-ptns rest-params) (<split-lambdalist> params)))
    (let* ((m (length bind-ptns))
           (tmp-vars (freplicate m #'gensym)))
      `((,@tmp-vars ,@rest-params)
        (bind ,(mapcar #'list bind-ptns tmp-vars)
          ,@body)))))

(defmacro lambda/bind ((&rest params) &body body)
  (cons 'LAMBDA (<bind-make-defs> params body)))
(defmacro named-lambda/bind (ident (&rest params) &body body)
  (list* 'NAMED-LAMBDA ident (<bind-make-defs> params body)))
(defmacro defun/bind (ident (&rest params) &body body)
  (list* 'DEFUN ident (<bind-make-defs> params body)))


(defun <flet/labels-transform> (op defs body)
  (dolist (d defs)
    (unless (and (proper-list-p d)
                 (>= (list-length d) 2)
                 (symbolp (first d))
                 (proper-list-p (second d)))
      (error "~A/BIND: illegal clause(s): ~A" op d)))
  (list* op
         (mapcar (bind:lambda-bind ((name ll . d)) (cons name (<bind-make-defs> ll d))) defs)
         body))


(defmacro flet/bind ((&rest flet-defs) &body body)
  (<flet/labels-transform> 'FLET flet-defs body))
(defmacro labels/bind ((&rest labels-defs) &body body)
  (<flet/labels-transform> 'LABELS labels-defs body))

;(bind (((:values a b) c)) a)
(defmacro let/bind ((&rest bindings) &body body)
  (let ((tmpvars (mapcar (lambda (b) (when (consp b) (gensym))) bindings)))
    `(LET ,(mapcan (lambda (b v) (when (consp b)
                                   (list v (second b))))
                   bindings tmpvars)
       (BIND ,(mapcar (lambda (b v) (if (consp b)
                                      (list (first b) v)
                                      b))
                      bindings tmpvars)
         ,@body))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#Comment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let/bind (((a . b) x)
           foo
           ((c . d) a)
           ((e . f) b))
  (list a b c))

(flet/bind ((a (b (c . d)) (list b c d)))
  (a 1 '(2 . 3)))

