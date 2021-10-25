;; -*- coding: utf-8 -*-
;; This file is part of CL-JKit.
;; Copyright (c) 2012-2021 tradcrafts

(jkit.core.init:define-package :jkit.core.test ()
  (:import/export :jkit.core.reader)
  (:use :cl :alexandria)

  (:export
   #:*ignore-testing*
   #:*force-testing* 
   #:enable-testing #:disable-testing
   #:testing

   ;; cl-annot annotations
   #:test #:verify #:todo
   #:eval-when-test
   
   ;;;;;
   #:todo..

   #:check-unit #:check-unit*
   #:check-units #:check-units*
   
   #:check-assert #:check-assert*

   #:precond 

   #:has-errors 
   #:has-no-errors
   #:protected-multiple-value-list #:protected-multiple-value-list*)

  )
