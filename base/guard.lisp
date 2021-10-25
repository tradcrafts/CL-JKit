;; -*- coding: utf-8 -*-
;; This file is part of CL-JKit.
;; Copyright (c) 2012-2021 tradcrafts

(jkit.core:jkit-core-header)

(jkit.core:define-package :jkit.base.guard* (:jkit.base.guard)
  (:use :cl)
  (:import/export :jkit.core)
  (:export
   #:guard-block
   #:gfail

   #:guard #:aguard
   #:gcond #:gacond
   #:gif #:gaif
   #:gwhen #:gawhen
   #:gunless
   #:gor  #:gtry
   #:gand #:gprogn
   #:gcase #:gacase

   #:gnot
   ))

(in-package :jkit.base.guard)

(defmacro <main-guard-block> (&body body)
  `(block |<main-guard-block>| ,@body))

(defmacro guard-block (&body body)
  `(block |guard-block| ,@body))

(defmacro <greturn> (form)
  `(return-from |<main-guard-block>| ,form))

(defmacro gfail ()
  '(return-from |guard-block| nil))
       

(defun <guard> (macroname and-op clauses)
  (unless (every #'consp clauses)
    (error "~D: illegal clause: ~D" macroname (find-if-not #'consp clauses)))           

  `(<main-guard-block>
    (or
      ,@(mapcar #/`(,and-op ,(car _) 
                    (guard-block (<greturn> (progn ,@(cdr _))) ))
                clauses)
    (gfail))))


(defmacro guard (&body clauses)
  (<guard> 'guard 'and clauses))
(defmacro aguard (&body clauses)
  (<guard> 'aguard 'aand clauses))

(defun <gif> (and-op test then else)
  (let ((main `(,and-op ,test (guard-block (<greturn> ,then)))))
    `(<main-guard-block>
      ,(if else
           `(unless ,main ,else)
           main))))

(defmacro gif (test then &optional else)
  (<gif> 'and test then else))
(defmacro gaif (test then &optional else)
  (<gif> 'aand test then else))
(defmacro gwhen (test &body body)
  (<gif> 'and test `(progn ,@body) nil))
(defmacro gawhen (test &body body)
  (<gif> 'aand test `(progn ,@body) nil))
(defmacro gunless (test &body body)
  (<gif> 'and `(not ,test) `(progn ,@body) nil))

(defun <gcond> (macroname gif-op clauses)
  (unless (every #'consp clauses)
    (error "~D: illegal clause: ~D" macroname (find-if-not #'consp clauses)))           

  (reduce (lambda (a b)
            `(,gif-op ,(car a)
              (progn ,@(cdr a))
              ,b))
          clauses
          :from-end t :initial-value nil))

(defmacro gcond (&body clauses)
  (<gcond> 'gcond 'gif clauses))
(defmacro gacond (&body clauses)
  (<gcond> 'gacond 'gaif clauses))


(defun <gcase> (macroname var keyform clauses)
  (unless (every #'consp clauses)
    (error "~D: illegal clause: ~D" macroname (find-if-not #'consp clauses)))           

  `(let ((,var ,keyform))
    ,(reduce (lambda (a b)
               `(gif ,(cond ((listp (car a))
                             `(or ,@(mapcar #/`(eql ,var ',_) (car a))))
                            ((member (car a) '(t otherwise))
                             t)
                            (t `(eql ,var ',(car a))))
                 (progn ,@(cdr a))
                 ,b))
             clauses
             :from-end t :initial-value nil)))

(defmacro gcase (keyform &body clauses)
  (<gcase> 'gcase (gensym) keyform clauses))
(defmacro gacase (keyform &body clauses)
  (<gcase> 'gacase 'it keyform clauses))

(defmacro gtry (&body forms)
  `(<main-guard-block>
    (or ,@(mapcar #/`(guard-block (<greturn> ,_))
                  forms))))

(defmacro gprogn (&rest forms)
  `(guard-block ,@forms))


(defmacro gand (&body forms)
  `(guard-block (and ,@forms)))

(defmacro gor (&rest forms)
  `(or ,@(mapcar #/`(guard-block ,_)
                 forms)))

(defmacro gnot (form &optional (ret-form t))
  `(let (success)
    (guard-block
      ,form
      (setq success t))
    (if success
      (gfail)
      ,ret-form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#Comment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      
(guard-block (gnot (gfail) (print 'ok)))


        

(gor
  (prog1 t (print 3)  (print 20))
  (print 2))

(prog1 (values 1 2) )

(defun foo (x) (prog1 nil (print x)))

(gacase 2 (nil 20) (1 30))
(gcase 2 (2 (guard (nil))) (2 4))
(ecase 3 ((3 8 6 7 0) 3) (3 30))

(guard)
(gcase x (a b) ((c f) d))

(defun foo (x) 
  (gacond ((symbolp x) (guard ((eq x 'foo) it)
                              (t 'bar)))
          ((numberp x) (guard ((> x 0) (gfail))
                              (t (- x))))))

(<greturn> gfail gescape

(time (foo 'x))
-3
3
3
(defun foo (x)
  (gcond (t nil)))


(defun foo (x)
  (gaif x
       (aguard ((consp x) 'c)
              ((numberp x) 0)
              ((symbolp x) 300))))

(agif 3)

(time (foo 'a))
300
(symbolp nil)
0
NIL
300
300
300
300
300
300



(time (block foo (return-from foo 2)))
2
300

(acase key (a b) (c d))

FOO 300

FOO
(??a ??b ??c)

(defmacro gand (&rest forms)
  `(and ,@(mapcar #/`(main-guard (test-guard (<greturn> ,_)) )
                  forms))))


(defun foo (val) (do-unify-if (val ??x) (print x) 'false))
(gunless nil
  (guard (nil 3)
         (nil 20)))
  
(do-unify 3 ?)

30 30
(gawhen 30 
  (print it)
  (guard ((> it 30) it)
         (t (1- it))))

(gand 1 2 3)


(gaif 3 
     (guard (nil 3) 
            (t (guard (nil 10)
                      ((symbolp 'a) @print it)))) 
     'else)



(gaif 3 (guard (t it)))


(defun foo () (return-from foo 3))






    

