;; -*- coding: utf-8 -*-
;; This file is part of CL-JKit.
;; Copyright (c) 2012-2021 tradcrafts

(jkit.core:jkit-core-header)

(jkit.core:define-package :jkit.base.hlist* (:jkit.base.hlist)
  (:use :cl)
  (:import/export :jkit.base.list*)
  (:export

   #:hcell ;; classname
   #:hcell-p
   #:hcell-value ;; data accessor
   
   #:hlist-p
   #:ensure-hlist
   #:hdirection
   
   #:make-hlist
   #:hlist ;; Function and Type
   #:copy-hlist

   #:hlist-to-list #:list-to-hlist
   #:unsafe-hlist-to-list #:unsafe-list-to-hlist

   #:hsingleton
   #:hcons

   ;; Accessors
   #:hcar #:hnth #:hlastcar
   #:hfirst #:hsecond #:hthird #:hfourth #:hfifth #:hsixth
   #:hseventh #:heighth #:hninth #:htenth

   #:hprev #:hturn #:hrewind

   #:do-hlist #:hmap

   #:hmember #:hmember-if #:hmember-if-not
   #:hposition #:hposition-if #:hposition-if-not
   #:hcount #:hcount-if #:hcount-if-not

   ;; 非破壊関数
   #:hreverse #:happend
   #:hremove #:hremove-if #:hremove-if-not
   #:hbutlast
   
   ;; 破壊関数
   #:hnconc
   #:hdivide
   #:hinsert #:hsplice
   #:hdelete #:hdelete-if #:hdelete-if-not
   #:hnbutlast
    ))
(in-package :jkit.base.hlist)



;; HLISTの要件
;; XがHLISTであるとは、(NULL X)もしくは(HCELL-P (CAR X))かつ
;; (OR (EQ X (HCELL-FWD (CAR X))) (EQ X (HCELL-FWD (CAR X))))が真
;; となることをいう

;; HLIST則
;; 以下の点が遵守されることを想定する
;; (1) HLISTのどのセルのCAR部CDR部ともに、値を直接書き換えてはならない
;; (2) HLISTを通常リストのCDR部に連結することは合法である
;; (3) 0 <= {i,j} <= (length hxs)とする。このときi/=jなら必ず(not (eq (car (nth i hxs)) (car (nth j hxs))))
;; (4) (3)から、(proper-list-p hxs)は常に真である。

;; 本体 CONSセルと相互リンクを形成することで双方向LISTとして作用する
(defstruct hcell fwd bwd value)

(defmethod print-object ((obj hcell) stream)
  (if (and (hcell-fwd obj) (hcell-bwd obj))
    (format stream "[~A]" (hcell-value obj))
    (format stream "{~A}" (hcell-value obj))))

;; todo describe-object
(defmethod describe-object ((obj hcell) stream)
  (declare (ignore obj))
  (format stream "HLIST"))



(defun hdirection (hxs)
  (flet ((err (x) (error "HDIRECTION: ~A is not a HLIST" x)))
    (cond ((null hxs) :forward)
          ;;;((hcell-p hxs) :forward)
          ((consp hxs)
            (let ((hl (car hxs)))
              (cond ((not (hcell-p hl)) (err hxs))
                    ((eq (hcell-fwd hl) hxs) :forward)
                    ((eq (hcell-bwd hl) hxs) :backward)
                    (T (err hxs)))))
          (T (err hxs)))))

;;(describe "foo")

;; hsingleton
@inline
(defun <singleton> (x)
  (let* ((s (make-hcell :value x))
         (fwd (cons s nil))
         (bwd (cons s nil)))
    (setf (hcell-fwd s) fwd
          (hcell-bwd s) bwd)
    (values s fwd bwd)))


(defun <error/wrong-direction> (op wrong-direction)
  (error "~A: wrong direction ~A" op wrong-direction))

(defun <error/hsingleton> (wrong-direction)
  (<error/wrong-direction> 'hsingleton wrong-direction))

@inline
(defun hsingleton (x &optional (direction :forward))
  (multiple-value-bind (s fwd bwd) (<singleton> x)
    (declare (ignore s))
    (cond ((eq direction :forward) fwd)
          ((eq direction :backward) bwd)
          (t (<error/hsingleton> direction)))))

#Test  hsingleton
(check-assert
  (eq :forward (hdirection (hsingleton 0)))
  (eq :forward (hdirection (hsingleton 0 :forward)))
  (eq :backward (hdirection (hsingleton 0 :backward))))
  

;; list 2 hlist
(defun <unsafe-list-to-hlist> (xs &optional reversed)
  (do ((cur xs (cdr cur))
       rev-prev)
      ((null cur) (values xs rev-prev))
       (let* ((s (setf (car cur) (make-hcell :value (car cur))))
              (rev (cons s rev-prev)))
         (if reversed
           (setf (hcell-bwd s) cur
                 (hcell-fwd s) rev)
           (setf (hcell-fwd s) cur
                 (hcell-bwd s) rev))
         (setq rev-prev rev))))

@inline
(defun unsafe-list-to-hlist (xs &optional (direction :forward))
  (<unsafe-list-to-hlist> xs (eq :backward direction)))

@inline
(defun list-to-hlist (xs &optional (direction :forward))
  (<unsafe-list-to-hlist> (copy-list xs) (eq :backward direction)))

;(hdirection (list-to-hlist (iota 6) ))

@inline
(defun make-hlist (size &key initial-element)
  (<unsafe-list-to-hlist> (make-list size :initial-element initial-element)))

@inline
(defun hlist (&rest xs)
  (unsafe-list-to-hlist xs))


;;@inline
(defun hlist-p (x)
  (or (null x)
      (and (consp x)
           (let ((hl (car x)))
             (and (hcell-p hl)
                  (or (eq (hcell-fwd hl) x)
                      (eq (hcell-bwd hl) x)))))))

(deftype hlist () `(satisfies hlist-p))

#Test type hlist
(check-assert (not (typep (list 1 2 3) 'hlist))
              (typep nil 'hlist)
              (typep (hlist 1 2 3) 'hlist))




;(typep nil 'hlist)

(defun <error/not-a-hlist> (x)
  (error "~A is not a HLIST" x))


;; リスト(セル)から構造体を取り出す
(defun <get> (x)
  (when x
    (if (consp x)
      (let ((s (car x)))
        (if (or (eq (hcell-fwd s) x)
                (eq (hcell-bwd s) x))
          s
          (<error/not-a-hlist> x)))
      (<error/not-a-hlist> x))))

;; 有効なHLISTであるかのチェック (不成立時はエラー)
;; 順方向なら:forward、逆方向なら:backwardを返す
(defun <verify> (x)
  (if (null x)
    :forward
    (if (consp x)
      (let ((s (car x)))
        (unless (hcell-p s) (<error/not-a-hlist> x))
        (cond ((eq (hcell-fwd s) x) :forward)
              ((eq (hcell-bwd s) x) :backward)
              (t (<error/not-a-hlist> x))))
      (<error/not-a-hlist> x))))



(defun <error/invalidate> (cell)
  (error "INVALID HLIST(CELL): ~A" cell))

(defmacro <validate/cell> (cell &optional direction)
  `(let* ((c ,cell)
          (hl (car c)))
     (or (hcell-p hl)
         (<error/invalidate> c))
     ,(cond
        ((not direction)
          '(if (or (eq c (hcell-fwd hl)) (eq c (hcell-bwd hl)))
            hl
            (<error/invalidate> c)))
        ((or (eq :forward direction) (eq :fwd direction))
          '(let ((fwd (hcell-fwd hl)))
            (if (or (eq c fwd) (eq c (hcell-bwd hl)))
              fwd
              (<error/invalidate> c))))
        ((or (eq :backward direction) (eq :bwd direction))
          '(let ((bwd (hcell-bwd hl)))
            (if (or (eq c bwd) (eq c (hcell-fwd hl)))
              bwd
              (<error/invalidate> c))))
        (T (error "CODING-ERROR: <validate/cell>")))))

(defmacro <validate/hl> (hxs &optional direction)
  `(let ((x ,hxs))
     (if (hcell-p x)
       ,(cond ((or (eq :fwd direction) (eq :forward direction))  '(hcell-fwd x))
              ((or (eq :bwd direction) (eq :backward direction)) '(hcell-bwd x))
              ((not direction) 'x)
              (T (error "CODING-ERROR: <validate/hl>")))
       (or (and (consp x) (<validate/cell> x ,direction))
           (<error/invalidate> x)))))


(defmacro <aget> (x &body body) `(let ((it (<get> ,x))) ,@body))
(defmacro <when> (x &body body) `(when ,x (<verify> ,x) ,@body)) 
(defmacro <fork> (x on-forward on-backward) `(if (eq :forward (<verify> ,x)) ,on-forward ,on-backward))
(defmacro <afork> (x on-forward on-backward)
  `(awhen ,x
     (if (eq :forward (<verify> it))
       (let ((it (car it))) (declare (type hcell it)) ,on-forward)
       (let ((it (car it))) (declare (type hcell it)) ,on-backward))))


(defun hequal (hxs-or-xs hys-or-ys &key (test #'eql) (start 0) end)
  (assert (and 'hequal
               (listp hxs-or-xs) (listp hys-or-ys)
               (or (null start) (non-negative-integer-p start))
               (or (null end) (non-negative-integer-p end))))

  (when (and start end)
    (assert (and 'hequal (<= start end))))
  (let ((xf (if (hlist-p hxs-or-xs) #'hcell-value #'identity))
        (yf (if (hlist-p hys-or-ys) #'hcell-value #'identity)))

    (do ((src (if start (nthcdr start hxs-or-xs) hxs-or-xs) (cdr src))
         (end/src (when end (nthcdr end hxs-or-xs)))
         (dst (if start (nthcdr start hys-or-ys) hys-or-ys) (cdr dst))
         (end/dst (when end (nthcdr end hys-or-ys)))
         )
        ((or (eq src end/src) (eq dst end/dst))
         (and (eq src end/src) (eq dst end/dst)))
      (unless (funcall test (funcall xf (car src)) (funcall yf (car dst)))
        (return)))))

#Test
(let ((a (hlist 1 2 3 4 5 6))
      (b (list  1 2 0 4 5 6)))
  (check-assert
    (hequal nil nil)
    (hequal a a) (hequal b b)
    (hequal a a) (hequal b b)
    (hequal (cdddr a) (cdddr b))
    (not (hequal nil a))   (not (hequal a nil))
    (not (hequal a b))     (not (hequal b a))
    (hequal a b :start 3)
    (hequal a b :end 2) (hequal a b :start 1 :end 2)
    (not (hequal a b :start 2))
    ))

@inline
(defun ensure-hlist (list)
  (if (hlist-p list)
    list
    (value/1 (list-to-hlist list))))

#Test ensure-hlist
(check-assert
  (null (ensure-hlist nil))
  (let ((src (hlist 1))) (eq src (ensure-hlist src)))
  (let* ((src (list 1))  (result (ensure-hlist src)))
    (and (not (eq src result)) (hlist-p result) (hequal src '(1)))))

;;; ACCESSORS: hcar hlastcar,hnth,first,...,tenth
@inline
(defun hcar (hxs)
  (<when> hxs (hcell-value (car hxs))))
@inline
(defun (setf hcar) (val hxs)
  (<verify> hxs)
  (setf (hcell-value (car hxs)) val))

     
@inline
(defun hlastcar (hxs)
  (<when> hxs (hcar (last hxs))))
@inline
(defun (setf hlastcar) (val hxs)
  (<verify> hxs)
  (setf (hcar (last hxs)) val))

@inline
(defun hnth (n hxs)
  (assert (and 'hnth (integerp n)))
  (<when> hxs (awhen (nth n hxs) (hcell-value it))))
@inline
(defun (setf hnth) (val n hxs)
  (assert (and 'hnth (integerp n)))
  (<verify> hxs)
  (setf (hcell-value (nth n hxs)) val))

#Test hlist accessors
(let ((src (hlist 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j)))
  (check-assert
    (eq nil (hcar nil))
    (eq nil (hlastcar nil))
    (eq nil (hnth 100 nil))
    (eq 'a (hcar src))
    (eq 'b (hnth 1 src))
    (eq nil (hnth 100 src))
    (eq 'j (hlastcar src))
    (= 0 (setf (hcar src) 0))
    (= 0 (hcar src))
    (= 1 (setf (hlastcar src) 1))
    (= 1 (hlastcar src))
    (= 2 (setf (hnth 5 src) 2))
    (= 2 (hnth 5 src))
    (hequal src '(0 b c d e 2 g h i 1))
    ;; error check
    (has-errors (setf (hcar nil) 0))
    (has-errors (setf (hlastcar nil) 0))
    (has-errors (setf (hnth 0 nil) 0))
    (has-errors (setf (hnth 10 src) 0))    
    ))

(eval-when (:compile-toplevel)
  (defmacro <wrapper> (fn native-fn)
    `(progn
       (declaim (inline ,fn (setf ,fn)))
       (defun ,fn (hxs) (<when> hxs (hcell-value (,native-fn hxs))))
       (defun (setf ,fn) (val hxs)
         (<verify> hxs)
         (setf (hcell-value (,native-fn hxs)) val))))
  )

(<wrapper> hfirst first) (<wrapper> hsecond second) (<wrapper> hthird third) (<wrapper> hfourth fourth)
(<wrapper> hfifth fifth) (<wrapper> hsixth sixth) (<wrapper> hseventh seventh) (<wrapper> heighth eighth)
(<wrapper> hninth ninth) (<wrapper> htenth tenth)

#Test hlist accessors(2) hfirst...htenth
(let ((src (hlist 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j)))
  (check-assert
    (eq nil (hfirst nil))
    (eq nil (htenth nil))
    (= 9 (setf (hfirst src) 9))   (= 9 (hfirst src))
    (= 8 (setf (hsecond src) 8))  (= 8 (hsecond src))
    (= 7 (setf (hthird src) 7))   (= 7 (hthird src))
    (= 6 (setf (hfourth src) 6))  (= 6 (hfourth src))
    (= 5 (setf (hfifth src) 5))   (= 5 (hfifth src))
    (= 4 (setf (hsixth src) 4))   (= 4 (hsixth src))
    (= 3 (setf (hseventh src) 3)) (= 3 (hseventh src))
    (= 2 (setf (heighth src) 2))  (= 2 (heighth src))
    (= 1 (setf (hninth src) 1))   (= 1 (hninth src))
    (= 0 (setf (htenth src) 0))   (= 0 (htenth src))
    (hequal src '(9 8 7 6 5 4 3 2 1 0))
    ;; error check
    (has-errors (setf (hfirst nil) 0))
    (has-errors (setf (htenth (cdr src)) 0))
    ))


;; 直前のconsを返す。端初ならNILである。
(defun hprev (hxs &optional (nth 1))
  (<afork> hxs
           (awhen (nthcdr nth (hcell-bwd it)) (hcell-fwd (car it)))
           (awhen (nthcdr nth (hcell-fwd it)) (hcell-bwd (car it)))))


;; 進行方向の逆転
(defun hturn (hxs)
  (<afork> hxs
           (values (hcell-bwd it) :backward)
           (values (hcell-fwd it) :forward)))

;; 先頭への巻き戻し
(defun hrewind (hxs)
  (<afork> hxs
           (aif (cdr (hcell-bwd it)) (hcell-fwd (lastcar it)) hxs)
           (aif (cdr (hcell-fwd it)) (hcell-bwd (lastcar it)) hxs)))
  
#Test for hprev hturn hrewind
(multiple-value-bind (src rev) (hlist 1 2 3)
  (check-assert (not (eq src rev))
                (hequal src '(1 2 3))  (hequal rev '(3 2 1))
                (null (hprev nil))
                (null (hprev src))
                (eq src (hprev (cdr src)))
                (hequal (hturn src) '(1))
                (eq src (hrewind (last src)))
                (eq rev (hturn (last src)))
                ))


(defmacro do-hlist ((var hlist-form &optional result-form) &body body)
  (let ((tmp (gensym))
        (hxs (gensym)))
    `(let ((,hxs ,hlist-form))
       (<verify> ,hxs)
       (dolist (,tmp ,hxs ,result-form)
       (let ((,var (hcell-value ,tmp)))
         ,@body)))))

#Test
(check-assert
  (eq nil (do-hlist (x (hlist 1 2))))
  (eq 'ok (do-hlist (x (hlist 1 2) 'ok)))
  (= 55 (let ((n 0))
          (do-hlist (i (unsafe-list-to-hlist (iota 10 :start 1)) n)
              (incf n i)))))

@inline
(defun hlist-to-list (hxs)
  (<verify> hxs)
  (mapcar #'hcell-value hxs))

#Test hlist-to-list
(check-assert  (has-errors (hlist-to-list '(1)))
               (null (hlist-to-list nil))
               (equal (hlist-to-list (hlist 1 2 3)) '(1 2 3)))

;; 分断
(defun hdivide (hxs)
  (let ((next (cdr hxs)))
    (cond (next
            (<afork> hxs
                     (setf (cdr (hcell-bwd (car next))) nil
                           (cdr (hcell-fwd it)) nil)
                     (setf (cdr (hcell-fwd (car next))) nil
                           (cdr (hcell-bwd it)) nil))
            (values hxs next))
          (t (values hxs nil)))))

#Test hdivide
(check-assert
  (null (hdivide nil))
  (bind ((src (cdr (hlist 0 1 2 3 4)))
         ((:values a b) (hdivide src)))
    (and (hequal a '(1))
         (hequal (hturn (last a)) '(1 0)) 
         (hequal b '(2 3 4))
         (hequal (hturn (last b)) '(4 3 2))))
  (bind ((src (hlist 1 2 3))
         ((:values a b) (hdivide (cddr src))))
    (and (hequal a '(3))
         (null b)
         (hequal src '(1 2 3))
         (hequal (hturn a) '(3 2 1))))
  )


;; destructiveが偽なら、hcellはリンクだけ切ってそのままリストの各要素として残す
;; デフォルトでは、各要素の値はhecllの格納値で置き換えられる
;; hxsの直前に要素があればhxsは自動的に分断されるので、位置について特に意識しなくてもよい
(defun unsafe-hlist-to-list (hxs &key (destructive t))
  (when hxs
    (<verify> hxs)
    (let ((xs (aif (hprev hxs) (value/2 (hdivide it)) hxs)))
      (do ((c xs (cdr c))
           last-back)
          ((null c) (values xs last-back))
        (let* ((hl (car c))
               (back (hcell-bwd hl))
               (val (hcell-value hl)))
          (when destructive
            (setf (car c) val
                  (car back) val
                  (hcell-value hl) val))
          (setf (hcell-fwd hl) nil
                (hcell-bwd hl) nil
                last-back back
                ))))))

#Test unsafe-hlist-to-list
(check-assert
  (null (unsafe-hlist-to-list nil))
  (bind ((src (hlist 1 2 3))
         ((:values fw bw) (unsafe-hlist-to-list src)))
    (and (eq src fw)
         (not (hlist-p fw))  (not (hlist-p bw))
         (equal fw '(1 2 3)) (equal bw '(3 2 1))))
  (bind ((src (hlist 1 2 3))
         ((:values fw bw) (unsafe-hlist-to-list (cdr src) :destructive nil)))
    (and (= 1 (length src))
         (not (hlist-p fw))  (not (hlist-p bw))
         (every #'null (maplist #'hlist-p fw))
         (every #'null (maplist #'hlist-p bw))
         (equal (mapcar #'hcell-value fw) '(2 3))
         (equal (mapcar #'hcell-value bw) '(3 2))))
  )

;(do-hlist (x (hlist 1 2 3)) (print x))

;; 最大count個までコピーする。fillが真かつcount > (length hxs)なら後続はpaddingで埋める
;; fillが偽ならcountと(length hxs)のいずれか短い個数となる
(defun copy-hlist (hxs &key ignore-direction count (fill t) padding)
  (let ((dir (<verify> hxs))
        (src-xs (copy-list* hxs :count count :fill fill :padding padding :filter #'hcell-value)))
    (<unsafe-list-to-hlist> src-xs (unless ignore-direction
                                     (eq :backward dir)))))

#Test copy-hlist
(check-assert
  (hequal (copy-hlist (hlist 1 2 3)) '(1 2 3))
  (hequal (copy-hlist (hlist 1 2 3) :count 5) '(1 2 3 nil nil))
  (hequal (copy-hlist (hlist 1 2 3) :count 5 :padding 'a) '(1 2 3 a a))
  (hequal (copy-hlist (hlist 1 2 3) :count 4 :fill nil) '(1 2 3))
  (hequal (copy-hlist (hlist 1 2 3) :count 2) '(1 2))
  )

;;(hcell-value (car (copy-hlist (hlist 1 2 3) :count 10)))


(defun <error/hcons> (x) (error "HCONS: ~A is" x))
(defun hcons (x hxs)
  (let ((s (<singleton> x)))
    (if hxs
      (<afork> hxs
               (if (cdr (hcell-bwd it))
                 (<error/hcons> hxs)
                 (progn (setf (cdr (hcell-fwd s)) hxs
                              (cdr (hcell-bwd it)) (hcell-bwd s))
                        (hcell-fwd s)))
               (if (cdr (hcell-fwd it))
                 (<error/hcons> hxs)
                 (progn (setf (cdr (hcell-bwd s)) hxs
                              (cdr (hcell-fwd it)) (hcell-fwd s))
                        (hcell-bwd s))))
      ;; hxs==NULL の場合
      (hcell-fwd s))))

#Testing hcons
(check-assert
  (let ((x (hcons 0 nil)))
    (and (eq :forward (hdirection x)) (hequal x '(0))))
  (let ((x (hcons 1 (hlist 2 3))))
    (and (eq :forward (hdirection x)) (hequal x '(1 2 3))))
  (let ((x (hcons 2 (list-to-hlist '(3 4) :backward))))
    (and (eq :backward (hdirection x)) (hequal x '(2 3 4))))
  ;; error check
  (has-errors (hcons 0 (cdr (hlist 1 2))))
  (has-errors (hcons 0 (cdr (list-to-hlist '(1 2) :backward))))
  )
  

(defun <nconc> (hxs hys)
  (let ((dir (<verify> hxs))
        (dir2 (<verify> hys)))
    (cond ((null hys) hxs)
          ((let ((sb (car hys)))
             (declare (type hcell sb))
             (rest (if (eq :forward dir2) (hcell-bwd sb) (hcell-fwd sb))))
            ;; hysが先頭を指していない
            (error "hys invalid: not a head"))
          ((null hxs) hys)
          ((not (eq dir dir2)) ;; 方向が違うHLISTを連結しようとした
            (error "direction mismatch"))
          (t (let* ((tail (last hxs)) 
                    (sa (car tail))
                    (sb (car hys)))
               (declare (type hcell sa sb))
               ;; カレント方向を連結
               (setf (cdr tail) hys)
               ;; 逆カレント方向を連結
               (if (eq :forward dir)
                 (setf (cdr (hcell-bwd sb)) (hcell-bwd sa))
                 (setf (cdr (hcell-fwd sb)) (hcell-fwd sa))))
            hxs))))





;; 非破壊
(defun hreverse (hxs &key ignore-direction)
  (let ((dir (<verify> hxs))
        (src-xs (hlist-to-list hxs)))
    (if (or ignore-direction (eq :forward dir))
      (<unsafe-list-to-hlist> (nreverse src-xs))
      (multiple-value-bind (fxs bxs) (<unsafe-list-to-hlist> src-xs)
        (values bxs fxs)))))

#Test hreverse
(check-assert
  (multiple-value-bind (fw bw) (hreverse (hlist 1 2 3))
    (and (eq :forward  (hdirection fw))  (eq :backward (hdirection bw))
         (hequal fw '(3 2 1))            (hequal bw '(1 2 3))))
  (multiple-value-bind (fw bw) (hreverse (list-to-hlist '(1 2 3) :backward))
    (and (eq :backward  (hdirection fw)) (eq :forward (hdirection bw))
         (hequal fw '(3 2 1))            (hequal bw '(1 2 3))))
  (multiple-value-bind (fw bw) (hreverse (list-to-hlist '(1 2 3) :backward) :ignore-direction t)
    (and (eq :forward  (hdirection fw))  (eq :backward (hdirection bw))
         (hequal fw '(3 2 1))            (hequal bw '(1 2 3))))
  (multiple-value-bind (fw bw) (hreverse nil)
    (and (null fw) (null bw))))

;; 非破壊
(defun happend (hxs hys &rest other-hlists)
  (let ((result (<nconc> (copy-hlist hxs) (copy-hlist hys))))
    (if other-hlists
      (happend result (reduce #'happend other-hlists :from-end t))
      result)))

(defun hnconc (hxs hys &rest other-hlists)
  (let ((result (<nconc> hxs hys)))
    (if other-hlists
      (hnconc result (reduce #'hnconc other-hlists :from-end t))
      result)))

#Test happend, hnconc
(let ((a (hlist 1 2))  (b (hlist 3 4))  (c (hlist 5 6)))
  (hequal (happend a b) '(1 2 3 4))
  (hequal (happend a b c) '(1 2 3 4 5 6))
  (hequal (happend nil c a nil b nil) '(5 6 1 2 3 4))
  (hequal (hnconc nil a) '(1 2))
  (eq a (hnconc nil a))
  (eq a (hnconc a nil))
  (hequal (hnconc a b) '(1 2 3 4))
  (hequal a '(1 2 3 4))
  (hequal b '(3 4))
  (hequal (hnconc a (hlist 'x 'y) c) '(1 2 3 4 x y 5 6))
  (hequal b '(3 4 x y 5 6))
  (hequal c '(5 6)))

;; (hturn (hlast (hnconc (hlist 1 2) nil (hlist 4) (hlist 10))))


    
;; (test (value/2 (hdivide (hturn (hlast (hlist 1 2 3))))))

(defun <hmap/overwrite> (f hxs)
  (<verify> hxs)
  (do ((cur hxs (cdr cur))
       prev)
      ((null cur) (values hxs (hturn prev)))
    (let ((s (car cur)))
      (setf (hcell-value s) (funcall f (hcell-value s))))
    (setq prev cur)))

;; overwriteが真のとき、ignore-directionは無視される
(defun hmap (f hxs &key ignore-direction overwrite)
  (when hxs
    (if overwrite
      (<hmap/overwrite> f hxs)
      (let ((dir (<verify> hxs))
            tmp)
        (do-hlist (x hxs (multiple-value-bind (rev result)
                             (<unsafe-list-to-hlist> tmp (or ignore-direction (eq :forward dir)))
                           (values result rev)))
          (push (funcall f x) tmp))))))

#Test hmap
(check-assert
  (null (hmap #'cons nil))
  (let* ((src (hlist 1 2 3))
         (result (hmap #'1+ src)))
    (and (not (eq src result))
         (eq :forward (hdirection result))
         (hequal result '(2 3 4))))

  (let* ((src (list-to-hlist '(1 2 3) :backward))
         (result/1 (hmap #'1- src))
         (result/2 (hmap #'1- src :ignore-direction t)))
    (and (not (eq src result/1)) (not (eq src result/2))
         (eq :backward (hdirection result/1))
         (eq :forward (hdirection result/2))
         (hequal result/1 '(0 1 2))
         (hequal result/1 result/2)
         ))
  )


(defun hsplice (hxs hys)
  (let ((dir (<verify> hxs))
        (dir2 (<verify> hys)))
    (when (cdr (hturn hys)) ;; 先頭でない
      (error "hsplite-err1"))
    (cond ((null hxs) hys)
          ((null hys) hxs)
          ((eq dir dir2)
            (multiple-value-bind (prev next) (hdivide hxs)
              (hnconc prev (hnconc hys next))))
          (t (error "hsplice")))))

#Test hsplice
(check-assert
  (null (hsplice nil nil))
  (let ((src (hlist 1))) (eq src (hsplice nil src)))
  (hequal (hsplice (hlist 1 2) (hlist 3 4)) '(1 3 4 2))
  ;; error check
  (has-errors (hsplice (hlist 1 2) (cdr (hlist 10 20))))
  )

(defun hinsert (hxs elem &rest other-elems)
  (let* ((dir (<verify> hxs))
         (hElems (hnconc (hsingleton elem dir)
                         (unsafe-list-to-hlist other-elems dir))))
    (multiple-value-bind (prev next) (hdivide hxs)
      (let ((new (hnconc prev hElems)))
        (if next
          (hnconc new next)
          new)))))

#Test hinsert
(check-assert
  (hequal (hinsert nil nil) '(nil))
  (let* ((src (hlist 1 2))
         (result (hinsert src 'a 'b 'c)))
    (eq src result)
    (hequal result '(1 a b c 2)))
  (let* ((src (hlist 1))
         (result (hinsert src 0)))
    (eq src result)
    (hequal result '(1 0))
    (hinsert src 'a)
    (hequal result '(1 a 0))))   

;;(hdirection (hinsert (list-to-hlist '(1 2 3) :backward) 0 1 2))


;;(defun test (x) (values (hrewind x) (hrewind (hturn x))))

(defun hbutlast (hxs &optional (n 1) &key ignore-direction)
  (let ((dir (<verify> hxs))
        (src (butlast hxs n)))
    (do ((cur src (cdr cur)))
        ((null cur)
         (<unsafe-list-to-hlist> src (not (or ignore-direction (eq :forward dir)))))
      (setf (car cur) (hcell-value (car cur))))))

#Test hbutlast
(let ((fw (hlist 1 2 3 4 5))
      (bw (list-to-hlist '(1 2 3 4 5) :backward)))
  (check-assert
    (null (hbutlast nil))
    (hequal (hbutlast fw) '(1 2 3 4))
    (hequal (hbutlast bw) '(1 2 3 4))
    (hequal (hbutlast bw 4) '(1))
    (eq :forward (hdirection (hbutlast fw)))
    (eq :forward (hdirection (hbutlast bw 1 :ignore-direction t)))
    (eq :backward (hdirection (hbutlast bw)))
    (null (hbutlast fw 5))
    (null (hbutlast fw 6))
    (hequal fw '(1 2 3 4 5))
    (hequal bw '(1 2 3 4 5))
    ))

(defun hnbutlast (hxs &optional (n 1))
  (when hxs
    (<verify> hxs)
    (aif (last hxs n)
         (let ((prev-of-last (hprev it)))
           (hdivide prev-of-last)
           (if (eq it hxs)
             (values nil nil)
             (values hxs (hturn prev-of-last))))
         ;; n==0 の場合
         (values hxs (hturn (last hxs))))))


#Test hnbutlast
(check-assert
  (null (hnbutlast nil))
  (null (hnbutlast nil 100))
  (let* ((whole (hlist 0 1 2 3 4 5 6))
         (src (cdr whole)))
    (and (hequal (hnbutlast src) '(1 2 3 4 5))
         (hequal (hnbutlast src 3) '(1 2))
         (hequal (hnbutlast src 3) nil)
         (hequal whole '(0))
         ))
  (let* ((whole (hlist 0 1 2 3 4 5 6))
         (src (cdr whole)))
    (and (hequal (value/2 (hnbutlast src)) '(5 4 3 2 1 0))
         (hequal (value/2 (hnbutlast src 3)) '(2 1 0))
         (hequal (value/2 (hnbutlast src 2)) nil)
         (hequal whole '(0))
         ))
  )



(defun <prepare-delete> (hxs dir)
  (when hxs
    (let* ((fwd? (eq :forward dir))
           (prev (cadr (if fwd? (hcell-bwd (car hxs)) (hcell-fwd (car hxs)))))
           )
      (do ((cur hxs (cdr cur))
           last)
          ((null cur) (values prev (car last)))
        ;; カレント方向側の相互リンクを全て切る (後で削除されたアイテムの目印となる)
        '(if fwd?
          (setf (hcell-fwd (car cur)) nil)
          (setf (hcell-bwd (car cur)) nil))
        (setq last cur))      
      )))


(defvar *<predicate>*)
(defun <fn/predicate> (s) (funcall *<predicate>* (hcell-value s)))
(defvar *<key>*)
(defun <fn/key> (s &aux (x (hcell-value s))) (if *<key>* (funcall *<key>* x) x))

(macrolet ((body (&body main)
             `(let ((dir (<verify> hxs))
                    (*<key>* key))
                (declare (ignorable dir))
                (if key
                  (setf (getf args :key) '<fn/key>)
                  (setq args (nconc args '(:key <fn/key>))))
                ,@main)))
  (defun hmember (item hxs &rest args &key key test test-not)
    (declare (ignorable test test-not))
    (body (apply #'member item hxs args)))
  (defun hmember-if (test hxs &rest args &key key)
    (body (apply #'member-if test hxs args)))
  (defun hmember-if-not (test hxs &rest args &key key)
    (body (apply #'member-if-not test hxs args)))
  (defun hposition (item hxs &rest args &key from-end (start 0) end key test test-not)
    (declare (ignorable from-end start end test test-not))
    (body (apply #'position item hxs args)))
  (defun hposition-if (test hxs &rest args &key from-end (start 0) end key)
    (declare (ignorable from-end start end))
    (body (apply #'position-if test hxs args)))
  (defun hposition-if-not (test hxs &rest args &key from-end (start 0) end key)
    (declare (ignorable from-end start end))
    (body (apply #'position-if-not test hxs args)))

  (defun hcount (item hxs &rest args &key (start 0) end key (test #'eql) test-not)
    (declare (ignorable start end test test-not))
    (body (apply #'count item hxs args)))
  (defun hcount-if (test hxs &rest args &key (start 0) end key)
    (declare (ignorable start end))
    (body (apply #'count-if test hxs args)))
  (defun hcount-if-not (test hxs &rest args &key (start 0) end key)
    (declare (ignorable start end))
    (body (apply #'count-if-not test hxs args)))

  (macrolet ((rem/body (main)
               `(body (remf args :ignore-direction)
                      (let ((xs ,main))
                        (do ((cur xs (cdr cur)))
                            ((null cur))
                          (setf (car cur) (hcell-value (car cur))))
                        (<unsafe-list-to-hlist> xs
                                                (not (or ignore-direction (eq :forward dir))))))))
    (defun hremove (item hxs &rest args &key ignore-direction from-end
                         (start 0) end (test #'eql) test-not count key)
      (declare (ignorable from-end start end count test test-not))
      (rem/body (apply #'remove item hxs args)))  
    (defun hremove-if (test hxs &rest args &key ignore-direction from-end (start 0) end count key)
      (declare (ignorable from-end start end count))
      (rem/body (apply #'remove-if test hxs args)))
    (defun hremove-if-not (test hxs &rest args &key ignore-direction from-end (start 0) end count key)
      (declare (ignorable from-end start end count))
      (rem/body (apply #'remove-if-not test hxs args))))

  (macrolet ((del/body (main)
               `(body (let* ((fwd? (eq :forward dir))
                            (begin (cadr (if fwd? (hcell-bwd (car hxs)) (hcell-fwd (car hxs)))))                     
                            (ys ,main))
                       (when begin
                         ;;直前要素へのつなぎ直し（直前要素が存在する場合＝hxsが始端でない場合）
                         (if fwd?
                           (setf (cdr (hcell-fwd begin)) ys)
                           (setf (cdr (hcell-bwd begin)) ys)))
                       ;; 繋ぎ直し (現時点でysはバラバラの構造となっていてHLISTの要件を満たしていない)
                       (do ((cur ys (cdr cur))
                            (prev (when begin (if fwd? (hcell-bwd begin) (hcell-fwd begin)))))
                           ((null cur) (values ys prev))
                         (let ((s (car cur)))
                           (if fwd?
                             (let ((bwd (hcell-bwd s)))
                               (setf (hcell-fwd s) cur
                                     (cdr bwd) prev
                                     prev bwd))
                             (let ((fwd (hcell-fwd s)))
                               (setf (hcell-bwd s) cur
                                     (cdr fwd) prev
                                     prev fwd)))))))))
               
    (defun hdelete (item hxs &rest args &key from-end (start 0) end (test #'eql) test-not count key)
      (declare (ignorable from-end start end count test test-not))
      (del/body (apply #'delete item hxs args)))
    (defun hdelete-if (test hxs &rest args &key from-end (start 0) end count key)
      (declare (ignorable from-end start end count))
      (del/body (apply #'delete-if test hxs args)))
    (defun hdelete-if-not (test hxs &rest args &key from-end (start 0) end count key)
      (declare (ignorable from-end start end count))
      (del/body (apply #'delete-if-not test hxs args)))))

#Test hmember hcount hposition
(let ((src (hlist 'a 'b 1 'c 'd 1)))
  (check-assert
    ;; for hmember
    (null (hmember 'a nil))
    (null (hmember 0 src))
    (eq src (hmember 'a src))
    (eq (nthcdr 4 src) (hmember 'd src))
    (null (hmember-if #'keywordp src))
    (eq (nthcdr 2 src) (hmember-if #'numberp src))
    (eq (nthcdr 2 src) (hmember-if-not #'symbolp src))
    ;; for hcount
    (eql 0 (hcount 'a nil))
    (eql 0 (hcount 0 src))
    (eql 1 (hcount 'a src))
    (eql 2 (hcount 1 src))
    (eql 0 (hcount-if #'symbolp nil))
    (eql 4 (hcount-if #'symbolp src))
    (eql 0 (hcount-if-not #'symbolp nil))
    (eql 2 (hcount-if-not #'symbolp src))
    ;; for hposition
    (null (hposition 'a nil))
    (null (hposition 'z src))
    (eql 0 (hposition 'a src))
    (eql 2 (hposition 1 src))
    (eql 5 (hposition 1 src :from-end t))
    (eql 2 (hposition-if #'numberp src))
    (eql 5 (hposition-if #'numberp src :from-end t))
    (eql 0 (hposition-if-not #'numberp src))
    (eql 4 (hposition-if-not #'numberp src :from-end t))
    ;; for hremove
    (null (hremove 'a nil))
    (hequal '(a b c d) (hremove 1 src))
    (hequal '(d c b a) (value/2 (hremove 1 src)))
    (hequal '(1 1) (hremove-if #'symbolp src))
    (hequal '(a b 1 c d) (hremove-if-not #'symbolp src :from-end t :count 1))
    (hequal '(1 c d 1) (hremove-if-not #'numberp src :count 2))
    (null (hremove-if #'identity src))
    ))

#Test hdelete
(let* ((whole (hlist 'a 1 2 3 4 5 6 7 8 9 0))
       (src (cdr whole)))
  (check-assert
    (eq src (hdelete 0 src))
    (hequal src '(1 2 3 4 5 6 7 8 9))
    (eq src (hdelete 5 src))
    (hequal src '(1 2 3 4 6 7 8 9))
    (eq src (hdelete-if #'evenp src))
    (hequal src '(1 3 7 9))
    (eq src (hdelete-if-not (lambda (x) (< x 4)) src))
    (hequal src '(1 3))
    (let ((r (cdr src)))
      (eq r (hdelete 1 src)))
    (hequal whole '(a 3))
    (null (hdelete-if #'numberp src))
    (hequal whole '(a))
    ))


  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#Commentaries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

         
(hdirection (hbutlast (list-to-hlist '(1 2 3) :backward) 0 :ignore-direction t))
(let ((x (list 1 2 3)))
  (eq x (butlast x 0)))


(nbutlast (list 1 2) 2)

(let ((x (list 1 2 3)))
  (eq x (nbutlast x 0)))

(last '(1 2 3) 4100)
(hnbutlast (cdr (hlist 1 2 3)) 3)
(last '(1 2 3) 0)

(copy-hlist (cdr (hlist 1 2 3)))

(let ((x (list 1 2 3)))
  (butlast x 30))

(hdivide (nthcdr 2 (hlist 1 2 3 4 5)))

(count 3 '(1 2 3))
(position-if)
(position-if-not)

(count-if (lambda (x) (= 3 x)) '(1 2 1 2 1 2 3 2 1 3) :from-end nil :start 9)
(member)
(hdelete 3 (hlist 1 2 3 4 ))
(hturn (hlast (hdelete 3 (cdr (hlist 3 1 3 23 0 3 3 3 )))))

(<prepare-delete> (identity (hlist 1 2 3)) :forward)


(defvar *<fn-test>*)
(defvar *<fn-test-not>*)



(member-if #'zerop '(1 2 3) :test (lambda (x y) (print (list x y))))



(hmember 2 (hlist 1 2 3) :test #'>)


(hmember-if #'evenp (hlist 1 2 3) :key #'-)

(position)

(hposition 2 (hlist 1 2 3 4))

(position-if)

(hposition-if (lambda (x) (print x) (= x 3)) (hlist 1 2 3 4) :from-end t)

(remove)

(hdirection (hremove 1 (list-to-hlist '(0 1 2 3 2 1 0 -1) :backward) :ignore-direction t))

(remove-if)

(hremove-if #'oddp (hlist 1 2 3 4 5 6))

(delete-if
(defun foo (&rest all &key igig)
  (list all igig))
(foo :igig nil)

(let ((xs '(1 2 3 4)))
  (setf (getf xs 100) 3)
  xs)

(let ((xs '(1 2 3 4)))
  (remf xs 1)
  

(member 1 '(1 2 3) :key #'identity :key 'identityafoo)

(defun foo (x &rest all &key y z)
  (list x y all))
(foo 1 :y 100 :z 20)

(defvar *last* 0)
(defun tes1 (x &optional (y 0) (z 0))
  (setq *last* (+ x y z)))
(defun tes2 (x &key (y 0) (z 0))
  (setq *last* (+ x y z)))

(defun tes3 (x &rest r)
  (setq *last* (if r
                 (+ x -1000000 (car (iota x)))
                 (+ x x))))


(defun sp (f n)
  (dotimes (i n) (funcall f i)))

(time (sp #'tes1 100000000))
(time (sp #'tes2 100000000))
(time (sp #'tes3 100000000))


(remove )
(remove-if (lambda (&rest x) (print x)) (iota 10) :start 3)
(defun hremove

       
(let ((xs (list 1 2 3)))
  (eq xs (delete 2 xs )))

(hdirection (hmap #'- (list-to-hlist '(1 2 3) :backward)))

(hdirection (hcons 10 (list-to-hlist '(1 2 3) :reverse t)))



(hsingleton 0 :backward)
;;(test (<tes> (list 1 2 3 4 5 6 7 8)))
;;(hturn (hlast (<tes> (list 1 2 3 4 5 6 7 8 9))))
;;(hturn (hlast (<tes> (list 1 2 3 4 5 6 7 8 9) t)))
    
(test (<nconc> (hlist 1 2 3) (hlist 4 5 6)))
(test (<nconc> (hturn (hlast (hlist 1 2 3))) (hturn (hlast (hlist 4 5 6)))))
(<verify> (<nconc> (hturn (hlast (hlist 1 2 3))) (hturn (hlast (hlist 4 5 6)))))
(<nconc> (hlist 1 2) (hlist 3))


(value/2 (hlist 1 2 3 3 4))
(<verify> (copy-hlist (value/1 (list-to-hlist '(1 2 3 4)))))



(<verify> (hreverse (value/2 (list-to-hlist '(1 2 3 4))) :ignore-direction t))

(<verify> (hreverse (value/2 (list-to-hlist (list 1 2 3 'a 'b))) :ignore-direction nil))

(unsafe-list-to-hlist (list 1 2 3) :reverse t)



  (assert (and 'copy-hlist (proper-list-p hxs)))
  (unsafe-list-to-hlist (hlist-to-list hxs)))

  (hrewind (hnthcdr 0 (hlist 1 2 3)))
(hprev (hnthcdr 1 (hlist 1 2 3)))

(hcar (hcdr (hlist 1 2 3)))

(hcar (hlist 1 2 3))
(hnth 1 (hlist 1 2 3))
  

(<validate/cell> (hlist 3))

@inline
;;
(defun foobar (x) (+ x x x))

                                        ;(<validate> (hback (hlist 1)) :bwd)
;;(<validate/hl> (list 1) :bwda)  
         

;;(hdirection (hback (hlist 3)))
;;(hdirection (list (<singleton> 0)))
;;(<hlist-p> (<singleton> 3))

  
;(hdirection nil)

;;(hlist-p (hback (hlist 3)))

;;(disassemble 'hlist-p)

;(hlist-p (list (<singleton> 1)))



;; (defmacro <ensure> (var &body body)
;;   `(let ((,var (if (consp ,var) (car ,var) ,var)))
;;     ,@body))

;; (defmacro <when> (var  &body body)
;;   `(when ,var (<ensure> ,var ,@body)))

;; (defmacro <awhen> (exp  &body body)
;;   `(awhen ,exp (<ensure> it ,@body)))

;; (defmacro <if> (var then &optional else)
;;   `(if ,var (<ensure> ,var ,then) ,else))


                                                  
                              
;; (defun hfront (hxs) (<fwd> hxs))

;; (defun hfront (hxs) (<when> hxs (hcell-fwd hxs)))
;; (defun hback (hxs) (<when> hxs (hcell-bwd hxs)))


;; (defun hnext (hxs) (<when> hxs (awhen (hcell-fwd hxs) (second it))))
;; (defun hprev (hxs) (<when> hxs (awhen (hcell-bwd hxs) (second it))))

;; (let ((h (make-hcell)))
;;   (setf (hcell-fwd h) 0
;;         (hcell-bwd h) 1)
;;   h)




;(hfront (hconcat (hlist 1 2) (hlist 4 5)))
;(hfront (hconcat (hlist 1 2) nil))
;(hfront (hconcat nil (hlist 4 5)))
;(hfront (hconcat nil nil))
;(hfront (hconcat (hlist 1 2) (hlist 4 5) (hlist 0 -1) (hlist 'a 'b)))


;(hfront (value/2 (hdivide (hlist 1 2 3))))

; (hcell-fwd (hcons 3 (hcons 2 nil)))


;(hlist-to-list (hlist 1 2 3))

(defun bool-rand () (zerop (random 2)))


 (let* ((s (read-file-into-string "u:/files"))
        (lines (split-sequence #\Newline s))
        tmp)
   (dolist (x lines)
     (let* ((vec (jkit.base.text::text-scan  "(.*?)-(.*)" x :output :sub-strings-as-list))
            (author (first vec))
            (title (second vec)))
       (unless (member author tmp :test 'equalp)
         (push author tmp))))
   (write-string-into-file 
    (string-join (mapcar (lambda (a f) (format nil "move \"~A\"~%" a)) tmp))
    "u:/bat.bat"))

;;;;
(let* ((s (read-file-into-string "u:/files"))
        (lines (split-sequence #\Newline s))
        tmp)
   (dolist (x lines)
     (let* ((vec (jkit.base.text::text-scan  "(.*?)-(.*)" x :output :sub-strings-as-list))
            (author (first vec))
            (title (second vec)))
       (push (format nil "move \"~A\" \"~A\\[~A] ~A\"~%" x author author title)
             tmp)))

       (write-string-into-file (string-join tmp)
                               "u:/bat2.bat"
                               :if-exists :allow))

 
  

 
 
(jkit.base.text::text-scan  "(.*)-(.*)" "a-aa" :output :sub-strings)
 
