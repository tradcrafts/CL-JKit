;; -*- coding: utf-8 -*-
;; This file is part of CL-JKit.
;; Copyright (c) 2012-2021 tradcrafts

(jkit.base:define-package :jkit ()
  (:use :cl)
  (:nicknames :jk)
  (:import/export :jkit.base :jkit.algebraic.core :jkit.prolog)
  (:export
   #:full-mode
   )
  (:unexport
   #:jkit-base-header)
  )

(in-package :jkit)

(defmacro full-mode ()
  '(jkit.base:jkit-base-header))







