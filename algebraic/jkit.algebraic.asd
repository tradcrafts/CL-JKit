;; -*- coding: utf-8 -*-
;; This file is part of CL-JKit.
;; Copyright (c) 2012-2021 tradcrafts

(asdf:defsystem :jkit.algebraic
  :license "LLGPL"
  :depends-on (:jkit.base)
  :serial t
  :components (
               (:module "xdata"
                        :components
                        ((:file "package")
                         (:file "defs"))
                        )
               (:module "core"
                        :components
                        ((:file "package")
                         (:file "applicable")
                         (:file "data-1")
                         (:file "data-2")))
                        
               
               (:file "package")


               ))



