;; -*- coding: utf-8 -*-
;; This file is part of CL-JKit.
;; Copyright (c) 2012-2021 tradcrafts

(jkit.core:jkit-core-header)

(jkit.core:define-package :jkit.base.iterate* (:jkit.base.iterate)
  (:use :cl)
  (:import/export :jkit.base.lazy-list*)
  (:export
   #:iterator-p
   #:itor-begin #:itor-rbegin
   #:itor-incr
   #:itor-end-p #:itor-valid-p
   #:itor-get 
   #:itor-set #:itor-modify

   #:foreach
   #:do-each
  ))

(in-package :jkit.base.iterate)



(defgeneric iterator-p (x))
(defgeneric itor-begin (obj))
(defgeneric itor-rbegin (obj)) ;; optional (default = error)
(defgeneric itor-incr (itor)) ;; itorをそのまま返すこと
(defgeneric itor-get (itor))
(defgeneric itor-end-p (itor))
(defgeneric itor-valid-p (itor)) ;; optional デフォルトは(not (itor-end-p itor))
(defgeneric itor-set (itor x)) ;; optional (デフォルトではエラーを生じさせる) xを返すこと
(defgeneric itor-modify (itor func)) ;; optional (必要なら独自定義）
;; エラー関数群
(defgeneric error-itor-incr (itor))
(defgeneric error-itor-get (itor))
(defgeneric error-itor-set (itor))

(defsetf itor-get itor-set)

(defmethod iterator-p (x) @ignore x)
(defmethod itor-valid-p (itor) (not (itor-end-p itor)))
(defmethod error-itor-get (itor)
  (error "ITOR-GET: iterator ~D" itor))
(defmethod error-itor-incr (itor)
  (error "ITOR-INCR: iterator ~D" itor))
(defmethod error-itor-set (itor)
  (error "ITOR-SET: iterator ~D" itor))
 
(defmethod itor-set (itor x)
  @ignore x
  (error "ITOR-SET or (SETF ITOR-GET): ~D" itor))

(defmethod itor-modify (itor f)
  (itor-set itor (funcall f (itor-get itor))))

;; このitor-getのデフォルト動作は、イテレータが終端に達しているときに
;; (CALL-INCR-METHOD)で呼び出されるものである。

(defmethod itor-get (x) 
  (if (iterator-p x)
    (error "ITOR-GET: ITERATOR ~D: cannot geterence" x)
    (error "ITOR-GET: ~D is not a iterator" x)))

(defmethod itor-incr (x) 
  (if (iterator-p x)
    (error "ITOR-INCR: ITERATOR ~D: invalidated" x)
    (error "ITOR-INCR: ~D is not a iterator" x)))

(defmacro foreach ((var obj &optional result) &body body)
  #{let ((itor (gensym)))
  `(do ((,itor (itor-begin ,obj) (itor-incr ,itor))
        ,var)
    ((itor-end-p ,itor) 
     ,result)
    (setq ,var (itor-get ,itor))
    ,@body))

(defmacro do-each ((var itor &optional result) &body body)
  #{let ((tmp (gensym)))
  `(do ((,tmp ,itor (itor-incr ,tmp))
        ,var)
    ((itor-end-p ,tmp) 
     ,result)
    (setq ,var (itor-get ,tmp))
    ,@body))

(defun map-each! (obj f)
  (do ((itor (itor-begin obj) (itor-incr itor)))
      ((itor-end-p itor))
    (itor-modify itor f)))


;;;;;; For LIST    

(defstruct list-iterator xs)
(defmethod iterator-p ((itor list-iterator)) 
  @ignore itor
  t)
(defmethod itor-begin ((x list)) (make-list-iterator :xs x))
(defmethod itor-incr  ((itor list-iterator)) 
  (aif (list-iterator-xs itor) 
       (setf (list-iterator-xs itor) (&cdr it))
       (error-itor-incr itor))
  itor)
(defmethod itor-get ((itor list-iterator)) 
  (aif (list-iterator-xs itor) 
       (&car it) 
       (error-itor-get itor)))
(defmethod itor-end-p ((itor list-iterator)) 
  (null (list-iterator-xs itor)))
(defmethod itor-set ((itor list-iterator) x) 
  (aif (list-iterator-xs itor)
    (setf (car it) x) 
    (error-itor-set itor)))

;;;;; For Vector

(defclass vector-iterator () ((vec :initarg :vec) (n :initarg :n) (i :initarg :i)))

(defmethod itor-begin ((x vector))
  (make-instance 'vector-iterator :vec x :n (length x) :i 0))

(defmethod iterator-p ((x vector-iterator)) 
  @ignore x
  t)                     

(defmethod itor-incr ((x vector-iterator))
  #{let ((i (slot-value x 'i)))
  (if (< i (slot-value x 'n))
    (progn (setf (slot-value x 'i) (1+ i))
           x)
    (error-itor-incr x)))

(defmethod itor-get ((x vector-iterator))
  #{let ((i (slot-value x 'i)))
  (if (< i (slot-value x 'n))
    (aref (slot-value x 'vec) i)
    (error-itor-get x)))

(defmethod itor-end-p ((x vector-iterator))
  (>= (slot-value x 'i) (slot-value x 'n)))

(defmethod itor-set((itor vector-iterator) x)
  #{let ((i (slot-value itor 'i)))
  (if (< i (slot-value itor 'n))
    (setf (aref (slot-value itor 'vec) i) x)
    (error-itor-set itor)))

(defclass reversed-vector-iterator (vector-iterator) ())

(defmethod itor-rbegin ((x vector))
  #{let ((n (length x)))
  (make-instance 'reversed-vector-iterator :vec x :n n :i (1- n)))

(defmethod itor-end-p ((x reversed-vector-iterator))
  (> 0 (slot-value x 'i)))

(defmethod itor-incr ((x reversed-vector-iterator))
  #{let ((i (slot-value x 'i)))
  (if (> 0 i)
    (error-itor-incr x)
    (progn (setf (slot-value x 'i) (1- i))
           x)))


#Comment

(let ((x "hello"))
  (map-each! x 'char-upcase)
  x)
(do-each (x (itor-rbegin "foobar"))
  (print x))

(let ((x '(1 3)))
  (map-each! x '1-)
  x)
(itor-incr (itor-begin '(3 2 3)))
#S(LIST-ITERATOR :XS (2 3))
#S(LIST-ITERATOR :XS (3 2 3))












(itor-get nil)


(foreach (x '(1 a b))
  (print x))

(foreach (x "fあoobar")
  (print x))

