;; -*- coding: utf-8 -*-
;; This file is part of CL-JKit.
;; Copyright (c) 2012-2021 tradcrafts

(jkit.core:jkit-core-header)

(in-package :jkit.base.unify)




(defvar *collect-vars-func* nil)

;;　使われている変数を列挙する
;@eval-always
(defun collect-vars (info &rest ptns)
  (declare (special *vars*))
  (progv '(*vars* *avoid-vars*) '(nil nil)
    (dolist (p ptns) 
      (funcall *collect-vars-func* ptns info))
    ;; 列挙に際し、重複するシンボルは除去される
    (let (tmp)
      (mapc #/(pushnew (to-var _) tmp) *vars*)
      tmp)))
    


(defun collect-variable-if-possible (sym)
  (declare (special *vars* *avoid-vars*))
  (when (and (is-var? sym) 
             (not (eq (symbol-package sym) *pkg-unify-global-var*))
             (not (is-wildcard? sym)))
    (unless (or (member sym *avoid-vars*)
                (member sym *vars*))
      (push sym *vars*))))
  

(defun collect-vars-simply (x info)
  (cond ((consp x)
          (collect-vars-simply (car x) info)
          (collect-vars-simply (cdr x) info))
        ((simple-vector-p x)
          (dotimes (i (length x))
            (collect-vars-simply (svref x i) info)))
        ((symbolp x) (collect-variable-if-possible x))))

(defparameter *collect-vars-func* 'collect-vars-simply)

(defmacro let-for-unify-patterns (ptns &rest body)
  `(let (,@(apply #'collect-vars ptns))
    ,@body))
      
