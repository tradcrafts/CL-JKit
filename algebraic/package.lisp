;; -*- coding: utf-8 -*-
;; This file is part of CL-JKit.
;; Copyright (c) 2012-2021 tradcrafts

;; すこしHaskell的な、代数的データ型の実現
;; define-data,define-newtypeは、組み込み言語とのシームレスな接続を実現
;; define-internal-data,define-internal-newtypeは、CL内のみの使用を前提とした効率的な実装

(jkit.base:define-package :jkit.algebraic ()
  (:use :cl :jkit.base)
  (:import/export :jkit.algebraic.core :jkit.algebraic.xdata)
  (:export
   
   ))

