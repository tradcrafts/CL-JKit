;; -*- coding: utf-8 -*-
;; This file is part of CL-JKit.
;; Copyright (c) 2012-2021 tradcrafts

(jkit.core:jkit-core-header)
 
(jkit.core:define-package :jkit.base.text* (:jkit.base.text)
  (:use :cl :cl-ppcre)
  (:import/export :jkit.base.guard*)

  (:export
   #:=~ #:!~

   #:$ #:text-compile-literal

   
   #:text-scan
   #:text-matches #:text-unmatches
   #:text-replace
   #:text-split
   #:regex
   
   #:do-text-matches #:do-text-unmatches
   #:text-all-matches #:text-all-matches-as-strings
   #:text-all-unmatches #:text-all-unmatches-as-strings
   )
  )

(in-package :jkit.base.text)
