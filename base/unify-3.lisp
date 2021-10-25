;; -*- coding: utf-8 -*-
;; This file is part of CL-JKit.
;; Copyright (c) 2012-2021 tradcrafts

(jkit.core:jkit-core-header)

(in-package :jkit.base.unify)


(defmacro |do-unify-if| ((src ptn &key 
                              enumerated let let* import clone define 
                                        ;(on-success t) (on-failure nil)
                              (on-success t) (on-failure nil)
                              (success '|*unbound*|) (failure '|*unbound*|)
                              prepare
                              (test 'equal)(var-prefix #\?))
                         then-clause 
                         else-clause
                         &key
                         (guarded 'NONE))


  (unless (eq success '|*unbound*|) 
    (setq then-clause (list 'progn then-clause success)))
  (unless (eq failure '|*unbound*|) 
    (setq else-clause (list 'progn else-clause failure)))

  (when enumerated
    (unless (and (proper-list-p src)
                 (proper-list-p ptn)
                 (eql (length src) (length ptn)))
      (error "illegal pair of enumerated expression ~D & ~D" src ptn)))
  (when (and import (not (funcall 'valid-variables-list? import)))
    (error ":import"))
  (when (and clone (not (funcall 'valid-variables-list? clone)))
    (error ":clone"))
  (when (and let (not (funcall 'valid-let-head? let)))
    (error ":let"))
  (when (and let* (not (funcall 'valid-let-head? let)))
    (error ":let*"))
  (when (and let (not (funcall 'valid-define-list? define)))
    (error ":define"))

  (when (and let let*)
    (error "let and let* conflicts"))
  
  (when define
    (setq define (<transform-define-clauses> define)))


  ;; まずcollect-vars を呼んでパターン中の全ての表現を走査することで、
  ;; マクロ展開が必要な場合にはそれ用のハッシュテーブルがinfo中に用意される
  (let* ((info (make-uinfo :var-prefix var-prefix :test test))
         (all-vars (apply #'collect-vars info 
                          (if enumerated `(:AND ,@ptn) ptn)
                          (mapcar #'second define)))
         (initial-bindings (mapcar (lambda (v) (if (member v clone)
                                                 (list v v)
                                                 (list v ''*UNBOUND-UNIFICATION-VARIABLE*)))
                                   (set-difference (union clone all-vars) 
                                                   import))))

    (when define
      (setf (uinfo-labels info)
              (mapcar (lambda (d &aux (sym (if (symbolp (car d)) (car d) (caar d))))
                        (cons sym (internal-label sym)))
                      define)))

    (when (or import clone)
      (setq all-vars (union clone (union import all-vars))))

    (setf (uinfo-all-vars info) all-vars)

    (let* ((tmp (gensym))
           (main (if enumerated
                   `(and ,@(mapcar (lambda (s p) 
                                     `(let ((,tmp ,s)) (complex-unify ,tmp ,p ,info)))
                                   src ptn))
                   `(let ((,tmp ,src)) 
                     (complex-unify ,tmp ,ptn ,info)))))
      
      (when define
        #{let ((v (gensym)))
        (setq main `(labels ,(mapcar #/`(,(internal-label #>car) 
                                         (,v) 
                                         (complex-unify ,v ,#>second ,info))
                                     define)
                     ,main)))

      (unless (and (eq on-success t) (eq on-failure nil))
        (setq main (list 'if main on-success on-failure))) 

      (when prepare
        (setq main (list 'progn prepare main)))

      (let ((code (ecase guarded
                    (NONE (if else-clause
                            `(block |blk| 
                              (let (,@let ,@initial-bindings)
                                (if ,main
                                  (return-from |blk| ,then-clause)))
                              ,else-clause)
                            `(let (,@let ,@initial-bindings)
                              (if ,main
                                ,then-clause))))
                    (NORMAL (if else-clause
                              `(block |blk|
                                (let (,@let ,@initial-bindings)
                                  (if ,main 
                                    (guard-block (return-from |blk| ,then-clause))))
                                ,else-clause)
                              `(block |blk|
                                (let (,@let ,@initial-bindings)
                                  (if ,main 
                                    (guard-block (return-from |blk| ,then-clause)))))))
                    (SWAPPED `(block |blk|
                               (if (let (,@let ,@initial-bindings)
                                     (not ,main ))
                                 (guard-block (return-from |blk| ,then-clause)))))

                    )))
        (if let*
          (list 'let* let* code)
          code)
        ))))

(defmacro do-unify-if ((src ptn &rest options)
                       then-clause 
                       &optional else-clause)
  `(|do-unify-if| (,src ,ptn ,@options) ,then-clause ,else-clause)) 

(defmacro do-unify-when (test-clause &body then-clauses)
  `(|do-unify-if| ,test-clause (progn ,@then-clauses) nil))

(defmacro do-unify-unless (test-clause &body then-clauses)
  `(|do-unify-if| ,test-clause nil (progn ,@then-clauses)))

(defmacro do-unify-gif ((src ptn &rest options)
                       then-clause 
                       &optional else-clause)
  `(|do-unify-if| (,src ,ptn ,@options) ,then-clause ,else-clause :guarded NORMAL))

(defmacro do-unify-gwhen (test-clause &body then-clauses)
  `(|do-unify-if| ,test-clause (progn ,@then-clauses) nil :guarded NORMAL))

(defmacro do-unify-gunless (test-clause &body then-clauses)
  `(|do-unify-if| ,test-clause (progn ,@then-clauses) nil :guarded SWAPPED))


(defmacro do-unify (src ptn &rest keys)
  `(do-unify-if (,src ,ptn ,@keys) t))

(defun is-do? (ptn)
  (do-unify-if (ptn ((:-> :eq :do) . ?))
     t
     (do-unify-if (ptn ((:-> :eq :->) ? :unify ?p))
        (is-do? p)
        (do-unify-if (ptn ((:-> :eq :->) :unify ?p))
           (is-do? p)))))



;;


