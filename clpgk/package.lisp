;; -*- coding: utf-8 -*-
;; This file is part of CL-JKit.
;; Copyright (c) 2012-2021 tradcrafts

(jkit.base:define-package :jkit ()
  (:use :cl)
  (:import/export :jkit.base :jkit.algebraic.core :jkit.embed :jkit.prolog)
  (:export
   #:jkit-mode
   )
  (:unexport
   #:jkit-base-header)
  )

(in-package :jkit)

(defmacro jkit-mode ()
  `(jkit.base:jkit-base-header (:clap)))





