;; -*- coding: utf-8 -*-
;; This file is part of CL-JKit.
;; Copyright (c) 2012-2021 tradcrafts

(jkit.base:jkit-base-header)

(jkit.base:define-package :jkit.prolog.base ()
  (:use :cl :jkit.base :jkit.prolog.core)
  ;(:nicknames :pl :prolog)
  ;; (:import-from 
  ;;  :prolog-core
   
  ;;  #:*- #:*--
  ;;  #:!- #:!--


  ;;  #:pl-filter 

  ;;  #:define-pl-macro
  ;;  #:pl-genvar

  ;;  #:pl-clear
  ;;  #:pl-assert
  ;;  #:pl-asserta
  ;;  #:pl-retract

  ;;  #:pl-circulate

  ;;  #:pl-exec-1 #:pl-exec 
  ;;  #:pl-query-1 #:pl-query #:&pl-query 
  ;;  #:exec-prolog #:exec-prolog-1
  ;;  #:prolog #:prolog-1 #:do-prolog #:do-prolog-1 #:do-prolog-collect
  ;;  #:parallel-prolog #:parallel-prolog-1 
  ;;  #:do-parallel-prolog #:do-parallel-prolog-1 #:do-parallel-prolog-collect
   
  ;;  #:&prolog #:&do-prolog-collect
  ;;  #:&parallel-prolog #:&do-parallel-prolog-collect

  ;;  )

  (:export 
   #:pl-filter
   #:define-pl-macro #:pl-genvar
   #:pl-clear #:pl-retract
   #:pl-assert #:pl-asserta

   #:pl-circulate

   #:!- #:!--
   #:pl-exec-1 #:pl-exec 
   #:pl-query-1 #:pl-query #:&pl-query 
   #:exec-prolog #:exec-prolog-1
   #:prolog #:prolog-1 #:do-prolog #:do-prolog-1 #:do-prolog-collect
   #:parallel-prolog #:parallel-prolog-1 
   #:do-parallel-prolog #:do-parallel-prolog-1 #:do-parallel-prolog-collect
   
   #:&prolog #:&do-prolog-collect
   #:&parallel-prolog #:&do-parallel-prolog-collect

   #:*- #:*--
   #:**-
   #:pl. #:pl+
   ;#:deflop #:deflop*
   #:define-pl-op #:define-pl-op*
   
   #:**-
   #:**-*
   #:**-1 #:**-2 #:**-3 #:**-4 #:**-5 #:**-6

   #:!!-
   #:!!-1 #:!!-2 #:!!-3 #:!!-4 #:!!-5 #:!!-6


   ))

(in-package :jkit.prolog.base) 


(defun <define-pl-rule> (op redefine-p rule-name defs)
  `(prog1
     ',rule-name
     ,@(when redefine-p `((pl-clear ',rule-name)))
     ,@(mapcar (lambda (def)
                (if (listp def)
                  (bind (((head &rest rest) (split-sequence :- def)))
                    (when (< 1 (length rest))
                      (error "~A: Syntax Error: too much `:-' : ~D" op def))
                    `(*- (,rule-name ,@head) ,@(car rest)))
                  `(*- (,rule-name ,def))
                ))
              defs)))
  
(defmacro pl. (rule-name &body defs)
  (<define-pl-rule> 'pl. t rule-name defs))

(defmacro pl+ (rule-name &body defs)
  (<define-pl-rule> 'pl+ nil rule-name defs))


;; [2013-08-21]
(defmacro **- (rule-name &body defs)
  `(prog1
     ',rule-name
    (pl-clear ',rule-name)
    ,@(mapcar (lambda (def)
                (if (consp def)
                  (if (consp (car def))
                    `(*- (,rule-name ,@(car def)) ,@(cdr def))
                    `(*- (,rule-name ,(car def)) ,@(cdr def)))
                  `(*- (,rule-name ,def))))
              defs)))
  


(defun <make-variables> (from n)
  (loop for i from from to (1- (+ from n)) 
        collect (intern (format nil "?~A" i))))
    

(defmacro define-pl-op (name (n &rest params) &body body)
  (let ((ys (<make-variables> 0 n))
        (xs (<make-variables> n (length params)))
        (f (make-symbol "internal")))
    
    `(prog1
       ',name
      (setf (symbol-function ',f) (lambda (,@params) ,@body))
      ,(if ys
        `(pl. ,name (,@xs ,@ys :- (:is ,@ys (,f ,@xs))))
        `(pl. ,name (,@xs :- (:lop (,f ,@xs))))))))
    
(defmacro define-pl-op* (name (n &rest params) &body body)
  (let ((ys (<make-variables> 0 n))
        (xs (<make-variables> n (length params)))
        (f (make-symbol "internal")))
    `(prog1
       ',name
        (setf (symbol-function ',f) (lambda (,@params) ,@body))
        (pl-clear ',name)
        ,@ (with- (let (acc) + loop) 
             (push (if ys
                     `(*- (,name ,@xs ,@ys) (:is ,@ys (,f ,@xs)))
                     `(*- (,name ,@xs) (:lisp (,f ,@xs))))
                   acc)
             (if ys (pop ys) (return (nreverse acc)))))))
             
    



(defun probably-rule-name? (x)
  (do-unify x (:OR (:-> (atom))
                   ((:-> :type keyword) (:-> (atom))))))

(defun <**-*> (header xss &optional replace-p)
  (when (probably-rule-name? header)
    (setq header (list header)))
  (unless (every #'listp xss)
    (error "**-*: list required"))
  `(progn 
    ,@(when replace-p `((pl-clear ',(car header))))
    ,@(mapcar #/(list '*- (append header _)) xss)))

(defmacro **-* (header &body xss)
  (<**-*> header xss))


(defun <__-> (macro op header xs)
  #{let ((n (length xs))
         m)
  (when (or (probably-rule-name? header)
            (zerop (setq m (count '* header))))
    (error "~D: invalid header: ~D" macro header))
  (unless (zerop (mod n m))
    (error "~D: number of * is ~D but number of values is ~D" macro m n))
  #{let (tmp
         (cnt 0)
         h)
  (dolist (x xs `(prog1
                   ',(car header)
                  ,@(nreverse tmp)))
    (when (zerop cnt)
      (setq cnt m
            h (copy-list header))
      (push (list op h) tmp))
    #{let ((pos (position '* h)))
    (setf (nth pos h) x)
    (decf cnt)))


(defun <__-m> (macro op m rule-name xs)
  (unless (probably-rule-name? rule-name)
    (error "~D: ~D is not valid Rule-Name" macro rule-name))
  #{let ((n (length xs)))
  (unless (zerop (mod n m))
    (error "~D: number of values is ~D" macro n))
  (<__-> macro op (cons rule-name (replicate m '*)) xs))


(defmacro **- (header &body xs)
  (<__-> '**- '*- header xs))
(defun <**-m> (macro m rule-name xs)
  (<__-m> macro '*- m rule-name xs))
(defmacro **-1 (rule-name &body xs) (<**-m> '**-1 1 rule-name xs))
(defmacro **-2 (rule-name &body xs) (<**-m> '**-2 2 rule-name xs))
(defmacro **-3 (rule-name &body xs) (<**-m> '**-3 3 rule-name xs))
(defmacro **-4 (rule-name &body xs) (<**-m> '**-4 4 rule-name xs))
(defmacro **-5 (rule-name &body xs) (<**-m> '**-5 5 rule-name xs))
(defmacro **-6 (rule-name &body xs) (<**-m> '**-6 6 rule-name xs))

(defmacro !!- (header &body xs)
  (<__-> '!!- '!- header xs))
(defun <!!-m> (macro m rule-name xs)
  (<__-m> macro '!- m rule-name xs))
(defmacro !!-1 (rule-name &body xs) (<!!-m> '!!-1 1 rule-name xs))
(defmacro !!-2 (rule-name &body xs) (<!!-m> '!!-2 2 rule-name xs))
(defmacro !!-3 (rule-name &body xs) (<!!-m> '!!-3 3 rule-name xs))
(defmacro !!-4 (rule-name &body xs) (<!!-m> '!!-4 4 rule-name xs))
(defmacro !!-5 (rule-name &body xs) (<!!-m> '!!-5 5 rule-name xs))
(defmacro !!-6 (rule-name &body xs) (<!!-m> '!!-6 6 rule-name xs))




