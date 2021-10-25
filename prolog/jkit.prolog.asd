;; -*- coding: utf-8 -*-
;; This file is part of CL-JKit.
;; Copyright (c) 2012-2021 tradcrafts

(in-package :cl-user)

(asdf:defsystem :jkit.prolog  
  :license "LLGPL"
  :depends-on (:jkit.base)
  :serial t
  :components
  ((:file "0-prolog-core")
   (:file "1-prolog-base")
   (:file "package")
   (:file "pl-lib-1")
   ))




