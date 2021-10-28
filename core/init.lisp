;; -*- coding: utf-8 -*-
;; This file is part of CL-JKit.
;; Copyright (c) 2012-2021 tradcrafts

(defpackage :jkit.core.init
  (:use :cl)
  (:export
   #:define-package
   #:export*
   #:ensure-string
   ))

(in-package :jkit.core.init)

(defun ensure-string (src)
  (cond ((stringp src) src)
        ((symbolp src) (symbol-name src))
        (t (format nil "~A" src))))


(defgeneric <pkg-exports> (package))

(defmethod <pkg-exports> ((x package))
  (declare (ignore x))
  (warn "~D is not a CL-JKit:DEFINE-PACKAGE'ed package" (package-name *package*))
  nil)

(defmethod <pkg-exports> ((x (eql nil)))
  (declare (ignore x))
  (error "cl.unwired:define-package: undefined package")
  nil)

(defun <find-pkg> (pkgname-designator)
  (let ((pkg (find-package pkgname-designator)))
    (unless pkg (error "cl.unwired:define-package: package ~D not found" pkgname-designator))
    pkg))

(defun <name-eq> (a b)
  (string= (ensure-string a) (ensure-string b)))

;; 長さ1のnameには反応しないことに注意
(defun <unexport-by-initial-char> (ch name)
  (let ((s (ensure-string name)))
    (unless (and (> (length s) 1)
                 (eql (char s 0) ch))
      (list name))))


(defun <unexport-%> (name) (<unexport-by-initial-char> #\% name))
(defun <unexport-+> (name) (<unexport-by-initial-char> #\+ name))
(defun <unexport-*> (name) (<unexport-by-initial-char> #\* name))
(defun <unexport-$> (name) (<unexport-by-initial-char> #\$ name))
(defun <unexport-<> (name) (<unexport-by-initial-char> #\< name))
(defun <unexport-/> (name) (<unexport-by-initial-char> #\/ name))

(defmacro define-package (name (&body simple-defs) &body defs)
  (let (native-defs
        exclusive-defs
        use
        nicknames
        ;;import-only
        ;import/export
        ;import/export-from
        ;shadowing-import/export-from
        export
        extra-export
        unexport
        unexporters
        simple-package-name)

    (when simple-defs
      (setf simple-package-name (car simple-defs)))
    
    (dolist (d defs)
      (cond ((atom d)
              (cond ((eq :unexport-% d) (pushnew '<unexport-%> unexporters))
                    ((eq :unexport-$ d) (pushnew '<unexport-$> unexporters))
                    ((eq :unexport-< d) (pushnew '<unexport-<> unexporters))
                    ((eq :unexport-+ d) (pushnew '<unexport-+> unexporters))
                    ((eq :unexport-* d) (pushnew '<unexport-*> unexporters))
                    ((eq :unexport-/ d) (pushnew '<unexport-/> unexporters))
                    (t (push d native-defs))))
            ((eq :use (car d))
              (setf use (nconc use (copy-list (cdr d)))))
            ((eq :nicknames (car d))
              (setf nicknames (nconc nicknames (copy-list (cdr d)))))
            ((eq :unexport (car d))
              (setf unexport (nconc unexport (copy-list (cdr d)))))
            
            
            ((eq :export (car d))
              (setf export (nconc export (copy-list (cdr d)))))
            ;;((eq :import-only (car d))
            ;;  (setf import-only (nconc import-only (cdr d))))
            ((eq :import/export (car d))
              (setf use (append use (cdr d)))
              (setf extra-export (nconc extra-export
                                        (mapcan (lambda (x) (copy-list (<pkg-exports> (<find-pkg> x))))
                                                (cdr d)))))
            ((eq :import/export-from (car d))
              (push `(:import-from ,@(cdr d)) native-defs)
              (setf extra-export (nconc extra-export (copy-list (cddr d)))))

            ((or (eq :without-import-from (car d))
                 (eq :without-import/export-from (car d)))
              (let ((names (copy-list (<pkg-exports> (<find-pkg> (second d)))))
                    (dst (cddr d)))
                (setf names (delete-if #'(lambda (x) (member x dst :test #'<name-eq>))
                                       names))
                (push `(:import-from ,(second d) ,@names) native-defs)
                (when (eq :without-import/export-from (car d))
                  (setf extra-export (nconc extra-export names)))))


            ((eq :shadowing-import/export-from (car d))
              (push `(:shadowing-import-from ,@(cdr d)) native-defs)
              (setf extra-export (nconc extra-export (copy-list (cddr d)))))
            ((eq :nicknames (car d))
              (push d exclusive-defs))
            (t
              (push d native-defs))))
    (setf native-defs (nreverse native-defs))
    (when unexport
      (flet ((target? (x) (member x unexport :test #'<name-eq>)))
        (setf export (delete-if #'target? export)
              extra-export (delete-if #'target? extra-export))))
    (when unexporters
      (dolist (f unexporters)
        (setf export (mapcan f export)
              extra-export (mapcan f extra-export))))
    `(progn ;(eval-when (:compile-toplevel :load-toplevel :execute)

       ,@(when simple-defs `((defpackage ,simple-package-name
                               (:use ,@use)
                               ,@(cdr simple-defs)
                               ,@native-defs
                               (:export ,@export))))

       (defpackage ,name
         (:use ,@use ,@(when simple-defs (list simple-package-name)))
         (:nicknames ,@nicknames)
         ,@native-defs
         (:export ,@export ,@extra-export))

       
       (let* ((|head-| (list nil))
             (|tail-| (last |head-|))
             (|head+| (list* nil (copy-list ',extra-export)))
             (|tail+| (last |head+|))
             (|export| (copy-list ',export)))

         (DECLARE (IGNORABLE |head-| |head+| |tail-| |tail+|))
         (nconc |tail-| |export|)
         (nconc |tail+| |export|)
        
         ,@(when simple-defs `((defmethod <pkg-exports> ((x (eql (find-package ',simple-package-name))))
                                 (declare (ignore x))
                                 (values (rest |head-|)
                                         |tail-|))))
         
                              
         (defmethod <pkg-exports> ((x (eql (find-package ',name))))
           (declare (ignore x))
           (values (rest |head+|)
                   |tail+|)))
       ',name
       )))


(defun export* (sym-or-syms)
  (flet ((pushback-if-needed (x given)
           (unless (member (ensure-string x) (cdr given) :key #'ensure-string :test #'equal)
             (export x)
             (nconc given (list x)))))
         
    (multiple-value-bind (unuse given) (<pkg-exports> *package*)
      (DECLARE (IGNORE UNUSE))
      (cond (given
              (if (listp sym-or-syms)
                (dolist (x sym-or-syms)
                  (pushback-if-needed x given))
                (pushback-if-needed sym-or-syms given)))
              (t
               (warn "export*: ~D is not a CL-JKit:DEFINE-PACKAGE'ed package: use CL:EXPORT" (package-name *package*))
               (export sym-or-syms))))))
    
    
      

