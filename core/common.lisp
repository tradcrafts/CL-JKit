;; -*- coding: utf-8 -*-
;; This file is part of CL-JKit.
;; Copyright (c) 2012-2021 tradcrafts

(jkit.core.init:define-package :jkit.core.common ()
  (:use :cl)
  (:import/export :jkit.core.pre)
                                        ;(:import/export :anaphora :metabang-bind)
  ;(:import/export-from jkit.base.ext #:define-package #:string-concat #:memoized)
  ;(:import/export-from jkit.base.test #:enable-testing #:disable-testing #:precond)
  (:import/export-from anaphora #:it #:aif #:awhen #:acond #:acase #:alet)
  (:import/export-from anaphora
                       #:aand 		#:accase
                       #:actypecase 	#:aecase
                       #:aetypecase 	
                       #:aprog1 	#:asif 	#:atypecase
                       #:scase
                       #:sccase 	#:scond 	#:sctypecase
                       #:secase 	#:setypecase 	#:sif
                       #:slet 	#:sor 	#:stypecase
                       #:sunless 	#:swhen
                       )
  ;(:import/export-from annot.std #:export #:ignore #:ignorable)

  ;; (:import/export-from alexandria #:with-gensyms)
  ;; (:import/export-from alexandria #:proper-list-p #:circular-list-p #:circular-tree-p)
  ;; (:import/export-from alexandria #:proper-list #:circular-list)
  ;; (:import/export-from alexandria #:lastcar)
  ;; (:import/export-from alexandria #:hash-table-alist #:hash-table-plist
  ;;                      #:plist-hash-table #:alist-hash-table
  ;;                      #:maphash-keys #:maphash-values)
  ;; (:import/export-from alexandria #:copy-sequence #:emptyp #:alist-plist #:plist-alist)
  ;; (:import/export-from alexandria #:iota)
  ;; (:import/export-from alexandria #:ensure-symbol)
  ;; (:import/export-from alexandria #:deletef)
  ;; (:import/export-from alexandria #:negative-integer #:negative-integer-p
  ;;                      #:non-negative-integer #:non-negative-integer-p)
  (:import/export-from kmrcl #:ensure-string)
  (:import/export-from alexandria
                       #:alist-hash-table 	#:alist-plist #:appendf 	#:array-index
                       #:array-length 	#:assoc-value #:binomial-coefficient 	#:circular-list
                       #:circular-list-p 	#:circular-tree-p #:clamp 	#:coercef #:compose 	#:conjoin
                       #:copy-array 	#:copy-file #:copy-hash-table 	#:copy-sequence
                       #:copy-stream 	#:count-permutations #:cswitch 	#:curry #:define-constant 	#:delete-from-plist
                       #:delete-from-plistf 	#:deletef #:destructuring-case 	#:destructuring-ccase
                       #:destructuring-ecase 	#:disjoin #:doplist 	#:emptyp #:ends-with 	#:ends-with-subseq
                       #:ensure-car 	#:ensure-cons #:ensure-function 	#:ensure-functionf
                       #:ensure-gethash 	#:ensure-list #:ensure-symbol 	#:eswitch #:extremum 	#:factorial
                       #:featurep 	#:first-elt #:flatten 	#:format-symbol #:gaussian-random 	#:hash-table-alist
                       #:hash-table-keys 	#:hash-table-plist #:hash-table-values 	#:if-let
                       #:ignore-some-conditions 	#:iota #:last-elt 	#:lastcar #:lerp
                       #:make-circular-list 	#:make-gensym #:make-gensym-list 	#:make-keyword
                       #:map-combinations 	#:map-derangements #:map-iota 	#:map-permutations
                       #:map-product 	#:maphash-keys #:maphash-values 	#:mappend
                       #:maxf 	#:mean #:median 	#:minf
                       #:multiple-value-compose 	#:multiple-value-prog2
                       #:named-lambda 	#:nconcf
                       #:negative-double-float 	#:negative-double-float-p
                       #:negative-fixnum 	#:negative-fixnum-p #:negative-float 	#:negative-float-p
                       #:negative-integer 	#:negative-integer-p #:negative-long-float 	#:negative-long-float-p
                       #:negative-rational 	#:negative-rational-p #:negative-real 	#:negative-real-p
                       #:negative-short-float 	#:negative-short-float-p
                       #:negative-single-float 	#:negative-single-float-p
                       #:non-negative-double-float 	#:non-negative-double-float-p
                       #:non-negative-fixnum 	#:non-negative-fixnum-p #:non-negative-float 	#:non-negative-float-p
                       #:non-negative-integer 	#:non-negative-integer-p
                       #:non-negative-long-float 	#:non-negative-long-float-p
                       #:non-negative-rational 	#:non-negative-rational-p #:non-negative-real 	#:non-negative-real-p
                       #:non-negative-short-float 	#:non-negative-short-float-p
                       #:non-negative-single-float 	#:non-negative-single-float-p
                       #:non-positive-double-float 	#:non-positive-double-float-p
                       #:non-positive-fixnum 	#:non-positive-fixnum-p #:non-positive-float 	#:non-positive-float-p
                       #:non-positive-integer 	#:non-positive-integer-p
                       #:non-positive-long-float 	#:non-positive-long-float-p
                       #:non-positive-rational 	#:non-positive-rational-p #:non-positive-real 	#:non-positive-real-p
                       #:non-positive-short-float 	#:non-positive-short-float-p
                       #:non-positive-single-float 	#:non-positive-single-float-p
                       #:nreversef 	#:nth-value-or #:nunionf 	#:of-type #:once-only
                       #:parse-body 	#:parse-ordinary-lambda-list #:plist-alist 	#:plist-hash-table
                       #:positive-double-float 	#:positive-double-float-p #:positive-fixnum 	#:positive-fixnum-p
                       #:positive-float 	#:positive-float-p #:positive-integer 	#:positive-integer-p
                       #:positive-long-float 	#:positive-long-float-p #:positive-rational 	#:positive-rational-p
                       #:positive-real 	#:positive-real-p #:positive-short-float 	#:positive-short-float-p
                       #:positive-single-float 	#:positive-single-float-p 
                       #:proper-list 	#:proper-list-length #:proper-list-p 	#:proper-sequence
                       #:random-elt 	#:rassoc-value #:rcurry 	#:read-file-into-byte-vector #:read-file-into-string
                       #:read-stream-content-into-byte-vector #:read-stream-content-into-string
                       #:remove-from-plist 	#:remove-from-plistf #:removef 	#:required-argument #:reversef 	#:rotate
                       #:sequence-of-length-p 	#:set-equal #:setp 	#:shuffle
                       #:simple-parse-error 	#:simple-program-error #:simple-reader-error 	#:simple-style-warning
                       #:standard-deviation 	#:starts-with #:starts-with-subseq 	#:string-designator
                       #:subfactorial 	#:switch #:symbolicate 	#:type= #:unionf 	#:unwind-protect-case
                       #:variance 	#:when-let #:when-let* 	#:whichever #:with-gensyms 	#:with-input-from-file
                       #:with-output-to-file 	#:with-unique-names 
                       #:write-byte-vector-into-file 	#:write-string-into-file #:xor )

  (:export 

   ;; from COMMON-LISP
   #:in-package


           #:closure #:->
           #:isolate-without

           #:construct-instance
           #:define-instance-constructor

           


           #:jkit-core-header

           ;; annotations
           ;#:proc
           ))

(in-package :jkit.core.common)

(defmacro jkit-core-header (&rest ident-extra-readers)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (enable-reader ',ident-extra-readers)
    ;; 浮動小数点数表現は倍精度として読み込ませる
    ;(setq *read-default-float-format* 'DOUBLE-FLOAT)
    ;; 循環リストの表示を有効に
    ;(defparameter *print-circle* T)
    ))


;(defmacro proc (body) `(lambda () ,body))


;; make-instanceを使ったインスタンスの生成は、レストパラメータを取ることによって非常に不効率であるため
;; より効率的で高速なインスタンス生成マクロを提供する
;; (construct-instance クラス名 スロット1 値1 スロット2 値2 ...)

(defmacro construct-instance (&whole whole 
                              class &rest slot-and-values)
  (unless (evenp (length slot-and-values))
    (error "CONSTRUCT-INSTANCE: (length SLOT-AND-VALUES) must be even.~%but ~W"
           whole))
    
  `(let ((instance (make-instance ',class)))
    ,@(do ((c slot-and-values (cddr c))
           tmp)
          ((null c)
           (nreverse tmp))
          (let ((slot (first c))
                (value (second c)))
            (push `(setf (slot-value instance ',slot) ,value)
                  tmp)))
    instance))

;; インスタンスの生成関数の自動定義マクロ
;; (define-instance-constructor 生成される関数名 クラス名 スロット1 スロット2...)

(defmacro define-instance-constructor (constructor-name class &rest slots)
  (let (vars)
    (dotimes (i (length slots))
      (push (gensym) vars ))
    
    `(defun ,constructor-name ,vars
        (construct-instance ,class ,@(mapcan #'list slots vars)))))




