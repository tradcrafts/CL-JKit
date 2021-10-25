;; -*- coding: utf-8 -*-
;; This file is part of CL-JKit.
;; Copyright (c) 2012-2021 tradcrafts

;; #||#形式のコメントリーダの特殊コンバータ群の定義

(jkit.core:jkit-core-header)
(in-package :jkit.base.text)

(defun <string-to-lines> (source-string &key remove-spaces)
    (let ((lines (split-sequence #\Newline source-string)))
      (if remove-spaces
        (flet ((space? (x) (or (eq x #\Space) (eq x #\Tab))))
          (mapcar (lambda (s &aux (pos (position-if-not #'space? s)))
                    (cond ((not pos) (string ""))
                          ((eq 0 pos) s)
                          (t (subseq s pos))))
                  lines))
        lines)))

;; 最初の行を削除した残りの文字列を返す
(defun <op/rest-string> (source-string &aux (pos (position #\Newline source-string)))
  (if pos
    (subseq source-string (1+ pos))
    (string "")))
(defun <op/lines> (source &key *) (<string-to-lines> source :remove-spaces *))
(defun <op/rest-lines> (source &key *)
  (let ((s (<op/rest-string> source)))
    (when (not (equal "" s))
      (<string-to-lines> s :remove-spaces *))))

;(mapcan (lambda (x) (if x (list x x))) '(1 2 nil 3))
(defun <op/string*> (source &key *)
  (let ((lines (<op/lines> source :* t)))
    (apply #'string-concat
           (first lines)
           (mapcan (lambda (s) (list (memoized (coerce '(#\Newline) 'string)) s))
                   (cdr lines)))))


(define-comment-converter "string" (source)  source)
;(define-comment-converter "rest-string" (source) (<op/rest-string>  source))
;(define-comment-converter "string*" (source) (<op/string*>  source))
;(define-comment-converter "rest-string*" (source) (<op/string*> (<op/rest-string>  source)))
(define-comment-converter "lines" (source) (<op/lines>  source))
;(define-comment-converter "rest-lines" (source) (<op/rest-lines>  source))
;(define-comment-converter "lines*" (source)  (<op/lines> source :* t))
;(define-comment-converter "rest-lines*" (source)  (<op/rest-lines> source :* t))
;(define-comment-converter "read" (source)  (read-from-string source))

(define-comment-converter "regex" (source options "msie")
  (let ((code `(regex ,source
                      ,@(when (member #\m options) '(:multi-line-mode T))
                      ,@(when (member #\s options) '(:single-line-mode T))
                      ,@(when (member #\i options) '(:case-insensitive-mode T))
                      ,@(when (member #\e options) '(:extended-mode T)))))
      (list 'memoized code)))

(define-comment-converter "ppcre:scanner" (source options "msie")
  (let ((code `(create-scanner ,source
                               ,@(when (member #\m options) '(:multi-line-mode T))
                               ,@(when (member #\s options) '(:single-line-mode T))
                               ,@(when (member #\i options) '(:case-insensitive-mode T))
                               ,@(when (member #\e options) '(:extended-mode T)))))
    (list 'memoized code)))



(defun <read-file> (pathname options)
  (let ((encoding (to-external-format
                   (cond ((member #\a options) :ASCII)
                         ((member #\l options) :LATIN1)
                         ((member #\u options) :UTF8)
                         ((member #\s options) :SJIS)
                         ((member #\e options) :EUCJP)))))
    `(read-file-into-string ,pathname :external-format ,encoding)))
       
(define-comment-converter "file" (source options "baseul")
  (let ((pathname source))
    (if (member #\b options)
      ;; as BINARY FILE: ALL BYTES into BYTE-VECTOR
      `(read-file-into-byte-vector ,pathname)
      ;; as STRING
      (<read-file> pathname options))))


(define-comment-converter "read" (pathname options "aseul")
  `(read-from-string ,(<read-file> pathname options)))

(define-comment-converter "read-all" (pathname options "vaseul")
  (let ((code `(read-sequential-from-string ,(<read-file> pathname options))))
    (if (member #\v options)
      `(coerce ,code 'vector)
      code)))
  
; (define-comment-converter "PATHNAME" (pathname options "aseul")

(defmacro <show> (&rest xs &aux (n (length xs)))
  (case n
    (0  "")
    (1  `(format nil "~A" ,(first xs)))
    (t (let ((formatter (first xs)))
         (when (and (symbolp formatter)
                    (eq #\~ (char (symbol-name formatter) 0)))
           (setq formatter (format nil "~A" formatter)))
         `(format nil ,formatter ,@(rest xs))))))
                    

;; ヒアドキュメント
;; ネスト構造は不可である
(define-comment-converter "heredoc" (source options "pbB")
  (let (tmp
        (target source))
    (do-text ((x s e) "\\\\{(.*?)\\\\}" target :output :reacted-string :allow-unmatched t)
      (case x
        (:unmatched (push (subseq target s e) tmp))
        (t (push `(<show> ,@(read-sequential-from-string x)) tmp))))
    `(string-concat ,@(nreverse tmp))))


#Comment
'(#|`document!'hello\{(sqrt 2)\}\{"\\{"\}done|#)
'(#|`string*\n'
  hello\<Hello\>world\nok
  |#)
(text-matches "(l)" "hello" :output :region)
(let (tmp
      (target "he\\{ll\\}\\}o"))
  (do-text ((x s e) "\\\\{(.*?)\\\\}" target :output :reacted-string :allow-unmatched t)
    (case x
      (:unmatched (push (subseq target s e) tmp))
      (t (push x tmp))))
  (nreverse tmp))

'(#|`here_'hello\[\]\{"\\{"\}done|#)

'(#|`string*'
     ___Hello,World|#)

  
