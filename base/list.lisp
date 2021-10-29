;; -*- coding: utf-8 -*-
;; This file is part of CL-JKit.
;; Copyright (c) 2012-2021 tradcrafts

(jkit.core:jkit-core-header)

;; 競合するLENGTH=を退避
;;(unexport-and-escape 'alexandria:length=)

(jkit.core:define-package :jkit.base.list* (:jkit.base.list)
  (:use :cl)
;  (:import/export :jkit.base.member*)
  (:import/export :jkit.base.text*)
   (:export
     
     #:alist-p #:alist
     #:plist-p #:plist

     #:sublist-is-proper-p

     #:insert #:splice
     
     #:copy-list*
     #:length= #:length/= #:length< #:length<= #:length> #:length>=
     #:length<< #:length<<= #:length<=< #:length<=<=
     #:collect
     #:concat #:nconcat
     #:tails
     #:nub #:group
     #:filter #:filter-not
     #:intersperse
     #:intercalate
     #:zip #:unzip
     #:transpose
     #:replicate #:freplicate
     #:dolists #:dolists*
     #:nmapcar
     #:repeat #:cycle #:circulate
     
     #:maximum #:minimum
     #:words #:lines #:unwords #:unlines

     #:remove-from-alist #:delete-from-alist #:remove-from-alistf #:delete-from-alistf
     #:doalist

     #:recurs #:recurs*
     ))

(in-package :jkit.base.list)

(defun alist-p (x) (and (proper-list-p x)
                        (every #'consp x)))
(deftype alist () `(satisfies alist-p))

(defun plist-p (x) (and (proper-list-p x)
                        (evenp (length x))))
(deftype plist () `(satisfies plist-p))


;; リストの先頭から、指定された長さのサブリストを切り出せれば真
;; 長さが足りなかったり、非真リストにぶつかった場合は偽
;; (sublist-is-proper-p xs n)が真なら、(subseq xs 0 n)は安全に実行できる
(defun sublist-is-proper-p (xs sublist-length)
  (assert (and 'proper (listp xs) (non-negative-integer-p sublist-length)))
  (labels ((f (head x n)
             (cond ((zerop n) t)
                   ((atom x) nil)
                   (t (let ((rest (cdr x)))
                        (if (eq rest head)
                          t ;; 循環リストであることを検出した場合
                          (f head rest (1- n))))))))
    (f xs xs sublist-length)))

;; length= length/= length< length<= length> length>=
;; 長さの比較関数 リスト（循環リスト可）、またはシーケンスを取る
;; ただし、長さの値は正の整数のみとし、それ以外が与えられた場合の挙動は不定である
;; 循環リストの長さは∞として計算される

(defmacro <define-length-cmp> (fname cmp code-1 code-2 code-3 code-4)
  `(defun ,fname (n seq)
    (if (listp seq)
      (cond ((circular-list-p seq) ,code-1)
            ((zerop n) ,code-2)
            (seq (let ((it (nthcdr (1- n) seq)))
                   ,code-3))
            (t ,code-4))
      (,cmp n (length seq)))))


(<define-length-cmp> length= = nil (null seq) (and it (null (cdr it))) (zerop n))
(<define-length-cmp> length/= /= t (when seq t) (when (or (null it) (cdr it)) t) t)
(<define-length-cmp> length> > nil nil (null it) t)
(<define-length-cmp> length>= >= nil (null seq) (or (null it) (null (cdr it))) t)
(<define-length-cmp> length< < t (when seq t) (when (and it (cdr it)) t) nil)
(<define-length-cmp> length<= <= t t (when it t) nil)
(fmakunbound '<define-length-cmp>)

(defmacro <define-length/range> (name op-1 op-2)
  `(defun ,name (min max seq)
    (cond ((circular-list-p seq) nil)
          (t (let ((n (length seq)))
               (and (,op-1 min n) (,op-2 n max)))))))

(<define-length/range> length<=<= <= <=)
(<define-length/range> length<<   <  <)
(<define-length/range> length<=<  <= <)
(<define-length/range> length<<=  <  <=)
(fmakunbound '<define-length/range>)

;; `COPY-LIST*' 高性能なリスト複製関数
;; count個をコピーする。デフォルトではn==(length xs)
;; fillが真のとき、要素が余る場合にはpaddingを要素として不足分だけ挿入する
;; fillが偽なら、最大でcount個をコピーする
;; countが非NILの場合に限り、xsは循環リストであっても構わない。もしくは、xsのたかだか先頭count個がproperであれば良い
;; filterが指定された場合は、xsの各要素にfilterを適用した結果を新リストとする。
;; ただし、filterはpadding要素には適用されない
(defun copy-list* (xs &key count (fill t) padding filter)
  (macrolet ((f (x) `(let ((x ,x)) (if filter (funcall filter x) x))))
    (when count (assert (and 'copy-list* (non-negative-integer-p count))))
    (cond ((not count) ;;通常のコピー (要素数未指定)
            (if filter
              (mapcar filter xs)
              (copy-list xs)))
          (fill ;; count個をコピー（不足分は埋める）
            (let ((new (make-list count :initial-element padding)))
              (do ((src xs (cdr src))
                   (dst new (cdr dst)))
                  ((or (null src) (null dst)) new)
                (setf (car dst) (f (car src))))))
          (t ;; 最大でcount個をコピー
            (do (result
                 (src xs (cdr src)))
                ((or (null src) (> 0 (decf count)))
                 (nreverse result))
              (push (f (car src)) result))))))


#Test copy-list*
(check-assert
  (null (copy-list* nil))
  (null (copy-list* '(1 2 3) :count 0))
  (equal '(1 2 3) (copy-list* '(1 2 3)))
  (not (equal '(1 2 3) (copy-list* '(1 2 3 4))))
  (equal '(1 2 3) (copy-list* '(1 2 3 4) :count 3))
  (equal '(2 3 4 a a) (copy-list* '(1 2 3) :count 5 :padding 'a :filter #'1+))
  (equal '(0 1 2) (copy-list* '(1 2 3) :filter #'1-))
  (equal '(nil nil) (copy-list* nil :count 2))
  (equal '(0 nil) (copy-list* '(0) :count 2))
  (equal '(0 a) (copy-list* '(0) :count 2 :padding 'a))
  (equal '(0) (copy-list* '(0) :count 2 :padding 'a :fill nil))
  )


;; 要素の挿入(破壊的)
(defun insert (xs elem &rest other-elems)
  (cond ((null xs) (cons elem other-elems))
        ((consp xs)
          (if (null other-elems)
            (setf (cdr xs) (cons elem (cdr xs)))
            (setf (cdr xs) (cons elem (nconc other-elems (cdr xs)))))
          xs)
        (t (error "insert: ~A is not a list" xs))))

#Verify insert
(and (equal '(1 2 3 4 5) (insert (list 1 3 4 5) 2))
     (equal '(1 2 3 4 5) (insert (list 1 5) 2 3 4))
     (equal '(1) (insert nil 1))
     (equal '(1 2) (insert nil 1 2))
     )

;; リストの挿入(破壊的) 継ぎ接ぎされるysも破壊的に変更されることに注意
(defun splice (xs ys)
  (cond ((null xs) ys)
        ((consp xs) 
          (setf (cdr xs) (nconc ys (cdr xs)))
          xs)
        (t (error "splice: ~A is not a list" xs))))

#Verify splice
(and (equal '(1 3 4 2) (splice (list 1 2) (list 3 4)))
     (equal '(1 2) (splice nil (list 1 2)))
     (equal '(1) (splice (list 1) nil))
     (equal '(1 2) (splice (list 1) (list 2)))
     )


;; リストのリストを連結　完全非破壊
(defun concat (xss)
  (mapcan 'copy-list xss))

;; concatの破壊版
(defun nconcat (xss)
  (mapcan 'identity xss))

(defun <tails/haskell> (xs)
  (if xs
    (cons xs (<tails/haskell> (cdr xs)))
    (list nil)))
(defun <tails/lisp> (xs)
  (when xs
    (cons xs (<tails/lisp> (cdr xs)))))

;; TAILS tail部のリストを作る. :haskellが真の場合haskell流となる
(defun tails (xs &key haskell)
  (if haskell
    (<tails/haskell> xs)
    (<tails/lisp> xs)))


(defun intersperse (sep xs)
  (mapcon #/(if (cdr _) 
              (list (car _) sep) 
              (list (car _)))
          xs))

(defun intercalate (seps xss)
  (mapcon #/(if (cdr _) 
              (append (car _) (copy-list seps))
              (copy-list (car _)))
          xss))


(defun <transpose/truncate> (xss)
  (apply 'mapcar (lambda (&rest ys) ys) xss))

(defun <transpose> (xss)
  (let (result)
    (loop
      (let ((p xss)
            line)
        (while (setq p (member-if 'identity p))
          (push (caar p) line)
          (setq p (cdr p)))
        (unless line 
          (return-from <transpose> (nreverse result)))
        (push (nreverse line) result)
        (map-into xss 'cdr xss)))))
    
(defun transpose (xss &key truncate)
  (if truncate
    (<transpose/truncate> xss)
    (<transpose> xss)))

(defun zip (&rest xss)
  (<transpose/truncate> xss))

(defun <unzip> (xss m acc &aux (xs (car xss)))
  (cond (xs
          (assert (and 'unzip (= m (length xs))))
          (map-into acc 
                    (lambda (a x) (cons x a))
                    acc xs)
          (<unzip> (cdr xss) m acc))
        (t (map-into acc 'nreverse acc))))

;; (apply 'zip (unzip xs)) === xs
(defun unzip (xs)
  (when xs
    (let ((m (length (car xs))))
      (<unzip> xs m (make-list m)))))

(defmacro <circulate> (xs)
  `(let ((xs ,xs))
    (nconc xs xs)))

(defun circulate (xs)
;; リストの循環化。破壊的である。　xsが循環リストであれば、そのまま返す
  (if (circular-list-p xs)
    xs
    (<circulate> xs)))

(defun cycle (xs)
;; 循環リストの生成。非破壊。xsは既に循環リストであってもよく、その場合は再構成される。
  (if (circular-list-p xs)
    (do ((copied (list (car xs)))
         (begin xs)
         (xs (cdr xs) (cdr xs)))
        ((eq xs begin) (<circulate> (nreverse copied)))
      (push (car xs) copied))
    (<circulate> (copy-list xs))))

(defun replicate (n x &rest more-values)
  (assert (and 'replicate
               (integerp n)
               (<= 0 n)))
  (unless (zerop n)
    (if more-values
      (subseq (circulate (cons x more-values)) 0 n)
      (make-list n :initial-element x))))

(defmacro collect (&body body)
  `(with-collect (_) ,@body))

(defun <dolists> (macroname pairs body)
  (unless (<= 2 (length pairs))
    (error "~A: illegal variable clause ~D" macroname pairs))
  #{let ((ret))
  (when (oddp (length pairs))
    (setq ret (lastcar pairs))
    (nbutlast pairs))
  #{let* ((op-let (if (eq 'dolists macroname) 'let 'bind))
          (tmp (plist-alist pairs))
          (vars (mapcar 'car tmp))
          (lists (mapcar 'cdr tmp))
          (internals (freplicate (/ (length pairs) 2) #'gensym)))
  `(do ,(mapcar (lambda (internal list) `(,internal ,list (cdr ,internal)))
                internals lists)
       ((not (and ,@internals))
        ,@(when ret `((let ,vars (declare (ignorable ,@vars)) ,ret))))
    (,op-let ,(mapcar (lambda (var internal) `(,var (car ,internal)))
                  vars internals)
      ,@body)))

@doc"
;; 複数リストを扱えるようにした、DOLISTの拡張版 dolistのスーパーセットである
;; 変数はLETによって導入される（よって、var1をlist2から参照することは出来ない）
;; (dolists (var1 list1 var2 list2 ... [result]) ...)
"
(defmacro dolists ((&rest pairs) &body body)
  (<dolists> 'dolists pairs body))

@doc"
;; 複数リストを扱えるようにした、DOLISTの拡張版 dolistのスーパーセットである
;; 変数(群)はBINDによって導入される。直前のローカル変数の参照が可能である。
;; (dolists* (bindform1 list1 bindform2 list2 ... [result]) ...)
"
(defmacro dolists* ((&rest pairs) &body body)
  (<dolists> 'dolists* pairs body))

(defun <freplicate-1> (n f acc)
  (if (zerop n)
    (nreverse acc)
    (<freplicate-1> (1- n) f (cons (funcall f) acc))))
(defun <freplicate> (n fs acc)
  (if (zerop n)
    (nreverse acc)
    (let ((f (car fs))
          (fs (cdr fs)))
      (<freplicate> (1- n) fs (cons (funcall f) acc)))))
(defun freplicate (n f &rest more-fs)
  (assert (and 'freplicate (integerp n) (<= 0 n)))
  (if more-fs
    (<freplicate> n (circulate (cons f more-fs)) nil)
    (<freplicate-1> n f nil)))

(reduce #'min '(1 2 3) :initial-value 0)

;; MAPCARの自己破壊版
(defun nmapcar (f xs &rest others)
  (let* ((len (length xs))
         (cnt len))
    (dolist (list others)
      (setq cnt (min cnt (length list))))
    (apply 'map-into xs f xs others)
    (if (/= len cnt)
      (when (< 0 cnt)
        (setf (cdr (nthcdr (1- cnt) xs)) nil)
        xs)
      xs)))


(defun <filter> (f xs)
  (mapcan #/(when (funcall f _) (list _))
          xs))
(defun filter (f xs &key key)
  (if key
    (let ((g #/(funcall f (funcall key _))))
      (<filter> g xs))
    (<filter> f xs)))
(defun <filter-not> (f xs)
  (mapcan #/(unless (funcall f _) (list _))
          xs))
(defun filter-not (f xs &key key)
  (if key
    (let ((g #/(funcall f (funcall key _))))
      (<filter-not> g xs))
    (<filter-not> f xs)))


(defun repeat (x &rest more-values)
;; ひとつ（以上）の要素からなる循環リストの作成
  (let ((xs (list x)))
    (when more-values
      (setf (cdr xs) more-values))
    (nconc xs xs)))


(defun group (xs &key (test 'eql))
"
;; 「隣り合った」要素を順次比較し、比較関数が真となるものをリストにまとめる
;;  比較関数の引数には、左の要素→右の要素の順で渡される
;;  [x] -> [[x]]
"
  (when xs
    (let (result
          (cur (list (car xs))))
      (dolist (x (cdr xs) (nreverse (cons (nreverse cur) result)))
        (cond ((funcall test (car cur) x)
                (push x cur))
              (t 
                (push (nreverse cur) result)
                (setq cur (list x))))))))


(defun maximum (xs &key key)
  (if key
    (reduce #'max xs :key key)
    (reduce #'max xs)))
(defun minimum (xs &key key)
  (if key
    (reduce #'min xs :key key)
    (reduce #'min xs)))

(define-symbol-macro COMPOSED-TEST-KEY-FUNCTION
  (when (or test key)
     (cond ((and test key) 
             (closure (a b -> test key) (funcall test (funcall key a) (funcall key b))))
           (test
             (closure (a b -> test) (funcall test a b)))
           (key
             (closure (a b -> key) (eql (funcall key a) (funcall key b)))))))


(defun <nub-eql> (xs ys)
  (cond ((null xs) (nreverse ys))
        ((member (car xs) ys)
          (<nub-eql> (cdr xs) ys))
        (t (<nub-eql> (cdr xs) (cons (car xs) ys)))))
(defun <nub> (xs ys f)
  (cond ((null xs) (nreverse ys))
        ((member (car xs) ys :test f)
          (<nub> (cdr xs) ys f))
        (t (<nub> (cdr xs) (cons (car xs) ys) f))))

(defun nub (xs &key test key)
  (when xs
    (let ((f COMPOSED-TEST-KEY-FUNCTION))
      (if f
        (<nub> (cdr xs) (list (car xs)) f)
        (<nub-eql> (cdr xs) (list (car xs)))))))

(defun lines (s &key delims)
  (let* ((len (length s))
         (spaces (if delims delims '(#\newline #\linefeed #\return)))
         (r (split-sequence-if #/(member _ spaces)
                               s)))
    ;; 文字列の末尾が改行の場合、splitsequenceで切り出すと空文字列を入れて
    ;; くるので、結果からひとつ切り飛ばしてやる
    (if (and (< 0 len) 
             (member (char s (1- len)) spaces))
      (nbutlast r)
      r)))

(defun words (s &key delims)
  (let ((spaces (if delims delims '(#\space #\tab))))
    (split-sequence-if #/(member _ spaces)
                       s :remove-empty-subseqs t)))

(defun <unelems> (xs delim unlines?)
  (case (length xs) 
    (0 (make-string 0))
    (1 (copy-seq (car xs)))
    (t (let ((xs-len (length xs))
             (dst-len 0)
             (delim-len (length delim)))
         (dolist (x xs)
           (setq dst-len (+ dst-len (length x))))
         (setq dst-len (+ dst-len (* xs-len delim-len)))
         (unless unlines? (setq dst-len (- dst-len delim-len)))
         (let ((dst (make-string dst-len))
               (i 0))
           (dolist (x xs dst)
             (let ((x-len (length x)))
               (setf (subseq dst i) x)
               (when (or unlines? (not (zerop (decf xs-len))))
                 (setf (subseq dst (+ i x-len)) delim))
               (setq i (+ i x-len delim-len)))))))))

(defun unwords (xs &key (delim " "))
  (<unelems> xs delim nil))
(defun unlines (xs &key (delim #(#\newline)))
  (<unelems> xs delim t))

;; Alexandriaのdelete/remove-from-plistのalist版
;; Alexandriaに倣い、eq比較である
(defun delete-from-alist (alist &rest keys)
  (delete-if #/(member (car _) keys :test 'eq) 
             alist))
(defun remove-from-alist (alist &rest keys)
  (remove-if #/(member (car _) keys :test 'eq) 
             alist))
;; 上の２つのモディファイマクロ
(defmacro delete-from-alistf (place &rest keys)
  #{let ((tmp (gensym)))
  `(modifyf ,place (lambda (,tmp) (delete-from-alist ,tmp ,@keys))) )
(defmacro remove-from-alistf (place &rest keys)
  #{let ((tmp (gensym)))
  `(modifyf ,place (lambda (,tmp) (remove-from-alist ,tmp ,@keys))))

;; Alexandriaのdoplistのalist版
(defmacro doalist ((key val alist &optional resultform) &body body)
  #{let ((tmp (gensym)))
  `(dolist (,tmp ,alist ,resultform)
    (let ((,key (car ,tmp))
          (,val (cdr ,tmp)))
      (declare (ignorable ,key ,val))
      ,@body)))


;;;;;;; 漸化式 ;;;;;;;;

(defun <recurs> (n f init-value other-init-values)
  (assert (non-negative-integer-p n))
  (let (tmp)
    (if (null other-init-values)
      (let ((arg init-value))
        (do ((i n (1- i)))
            ((eql 0 i) (nreverse tmp))
          (push (setq arg (funcall f arg)) tmp)))
      (let ((args (cons init-value other-init-values)))
        (do ((i n (1- i)))
            ((eql 0 i) (nreverse tmp))
          (let ((a (apply f args)))
            (push a tmp)
            (setf (first args) a
                  args (rotate args -1))))))))

;; 漸化式による有限シーケンスの生成
(defun recurs (n f init-value &rest other-init-values)
  (nconc (cons init-value (copy-list other-init-values))
         (<recurs> n f init-value other-init-values)))
;; 漸化式による有限シーケンスの生成(ただし初項を含めない)
(defun recurs* (n f init-value &rest other-init-values)
  (<recurs> n f init-value other-init-values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; End Of J-LIST.LISP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
