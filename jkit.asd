;; -*- coding: utf-8 -*-
;; This file is part of CL-JKit.
;; Copyright (c) 2012-2021 tradcrafts

(asdf:defsystem :jkit
    :version "0.9"
    :description "CL-JKit: Common Lisp Junk Kit."
    :author ""
;    :depends-on (:jkit.core :jkit.base :jkit.algebraic :jkit.prolog :jkit.embed)
    :depends-on (:jkit.core :jkit.base :jkit.algebraic :jkit.prolog)
    :license "LLGPL"
    :serial t
    :components ((:file "package")
                 ))



