;; -*- coding: utf-8 -*-
;; This file is part of CL-JKit.
;; Copyright (c) 2012-2021 tradcrafts

(jkit.core:jkit-core-header)

(in-package :jkit.base.unify)


;@eval-always
(defun valid-let-head? (x)
  (do-unify x (:each (:and (:or (?v ?) (?v) ?v)
                           (:here (symbolp v))))))

(defun valid-variables-list? (x)
  (do-unify x (:each (:-> :type symbol))))


(defun valid-define-list? (x)
  (do-unify x (:each (:list* (:or (:-> :type symbol)
                                  (:LIST* (:-> :type symbol) (:-> :type proper-list)))
                             (:-> :type proper-list)))))

(defun valid-case-clause? (x)
  (do-unify x (:each (:list* ? (:-> :type proper-list)))))

(defun valid-append-clause? (x)
  (do-unify x
         (:each (:or (:-> :and listp );true-list?)
                     (:-> :view (x) (and (symbolp x) (is-var? x)))
                     (:-> :eq :maximize :minimize)))))

