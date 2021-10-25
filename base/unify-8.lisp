;; -*- coding: utf-8 -*-
;; This file is part of CL-JKit.
;; Copyright (c) 2012-2021 tradcrafts

(jkit.core:jkit-core-header)

(in-package :jkit.base.unify)


(defun <escaped?> (x) (and (consp x) (eq '|unif-quot| (car x))))
(defun <unescape> (x) (second x))
(defun <unescape-if-escaped> (x) (if (<escaped?> x) 
                                   (<unescape> x) 
                                   x))

(defmacro define-unification (rule-name type-tester &body body 
                              &aux inherits)
  (do-unify rule-name
    (:or (:-> :type symbol)
         (:each+ (:-> :type symbol)))
    :on-failure (error "DEFINE-UNIFICATION: illegal ~D" rule-name))

  (when (listp rule-name)
    (setf inherits (cdr rule-name)
          rule-name (car rule-name)))
  
  (do-unify-when
      (body
       (:and
         (:here (symbolp rule-name))
         (:each (:case 
                    (((:-> ?attr :eq :enum :bool :query :safe-query :bool-query) . ?r)
                     (:for r (:each (:call clause))))))
         (:for tmp (:each (:call pre-op))))
       :let (tmp names)
       :on-failure (error "match-rule def fail ~D" rule-name)
       :define
       (((clause :set :my r c)
         (:and ((:-> :type symbol) . ?r)
               (:each ?)
               ?c
               (:do (push `(,(car c) ,attr ,@(copy-list (cdr c))) tmp)
                    (push (car c) names)))) 
        ((pre-op :set :my vars r w q)
         (:case 
             ((? :enum  . ?r)
              (:do (map-into r (lambda (x) `(lambda (this) (declare (ignorable this)) ,x))
                             r)))
           ((:and ?w (? :bool . ?r) (:here (= 1 (length r)))
                  )
             (:do (map-into r (lambda (x) `(lambda (this) (declare (ignorable this)) (list 'if ,x t)))
                            r)))
             ;(:do (setf (cddr w) `((lambda (this &aux (code (progn ,@r))) `(when ,code t))))))
           ((:and ?w 
                  (? (:-> ?q :eq :query :safe-query :bool-query) ?vars . ?r) 
                  (:for vars (:each (:-> :type symbol))))
             (:do (setf (cddr w)
                          (let ((err (when (eq q :safe-query) '(error))))
                            `(,(length vars) (lambda (this ,@err ,@vars) (declare (ignorable this)) ,@r))))))))
         )
        )

    (let ((codes (mapcar (lambda (def)
                           `(list ',(first def) ,(second def) ,@(cddr def)))
                         tmp)))
      (unless inherits
        (setf codes (nconc codes '((list :this :enum 'identity)))))
      `(progn (setf (getf (symbol-plist ',rule-name) '<unify-rule>)
                      (list :tester (lambda (this) (declare (ignorable this)) ,type-tester)
                            :inherits ',inherits
                            :alist (list ,@codes)))
              ',rule-name)
      )))
        

(defun <rule-exist-p> (rule-name )
  (getf (symbol-plist rule-name) '<unify-rule>))

(defun <find-rule-type-tester> (rule-name)
  (awhen (getf (symbol-plist rule-name) '<unify-rule>)
    (getf it :tester)))

(defun <find-rule-alist> (rule-name cmd-name &optional (loop-detector-count 5000))
  (when (zerop loop-detector-count)
    ;; operation overfllow
    (error "unify: loop definition detected: ~D" rule-name))
  
  (let ((d (getf (symbol-plist rule-name) '<unify-rule>)))
    (when d
      (let ((alist (getf d :alist)))
        (if (assoc cmd-name alist)
          alist
          (dolist (parent-rule-name (getf d :inherits))
            (awhen (<find-rule-alist> parent-rule-name
                                      cmd-name
                                      (1- loop-detector-count))
            (return it))))))))


(defun <find-rule-command> (rule-name cmd-name)
  (awhen (<find-rule-alist> rule-name cmd-name)
    (values (assoc cmd-name it)
            it)))


(defun <generate-match-code> (rule-name v match-ptn info
                         &aux
                         ;(alist (<find-rule-alist> rule-name))
                         (tester (<find-rule-type-tester> rule-name)))
                         

  (do-unify-when
      (match-ptn
       (:call collect)
       :let (tmp 
             (v2 (gensym))
             )
       :define
       (((collect :set :my r args cmd)
         (:or nil
              (:and (:append :maximize
                             ((:-> ?cmd :view (c) (<find-rule-command> rule-name c)))
                             (:and (:each (:-> :view (c) (not (<find-rule-command> rule-name c))))
                                   ?args)
                             ?r)
                    (:do
                     (let* ((alist (<find-rule-alist> rule-name cmd))
                            (a (assoc cmd alist))
                            (mode (second a)))
                       
                       (unless a
                         (error "unknown ~D" cmd))
                       (case mode
                         (:enum
                           (when (< (length (cddr a)) (length args))
                             (error "ERRP ~D" cmd))
                           (push (mapcar (lambda (ptn f &aux (code (funcall f v)))
                                           ;;ワイルドカードの場合には、完全に無視する
                                           (if (and (symbolp ptn)
                                                    (is-var? ptn)
                                                    (is-wildcard? ptn))
                                             t
                                             `(let ((,v2 ,code))
                                                (complex-unify ,v2 ,ptn ,info))))
                                         args (cddr a))
                                 tmp))
                         (:bool
                           (when (< 1 (length args))
                             (error "ERRP bool ~D" cmd))
                           (when (null args) 
                             (setq args (list t)))
                           (let ((ptn (car args))
                                 (code (funcall (caddr a) v)))
                             (push `((let ((,v2 ,code))
                                       (complex-unify ,v2 ,ptn ,info)))
                                   tmp)))
                         ((:query :safe-query :bool-query)
                           '(unless (= (1+ (third a)) (length args))
                             (error "QUERY"))
                           (let* ((full? (let ((m (length args))
                                               (n (third a)))
                                           (case (- m n)
                                             (0 (when (eq mode :bool-query) 
                                                  (setq args (append args '(t)))
                                                  t))
                                             (1 t)
                                             (otherwise (error "QUERY")))))
                                  (f (fourth a))
                                  (xs (if full? (butlast args) args))
                                  (main (if full?
                                          `(complex-unify ,v2 ,(car (last args)) ,info)
                                          t)))
                             (when (member-if #'<escaped?> args)
                               (map-into xs #'<unescape-if-escaped> xs))
                             
                             (push (case mode
                                     (:query
                                       `((let ((,v2 ,(apply f v xs))) ,main)))
                                     (:bool-query
                                       `((let ((,v2 (if ,(apply f v xs) t))) ,main)))
                                     (t ; :safe-query
                                       `((let ((,v2 ,(apply f v <terr> xs)))
                                           (when (not (eq ,v2 <terr>)) ,main)))))
                                   tmp))))))
                    (:call collect r))
              (:-> :VIEW (rest) (error "PARSE ERROR ~D" rest))
              ))))
    (let ((test (funcall tester v)))
      `(and 
         ,test
         ,@(apply 'append (nreverse tmp))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;; END of J-UNIFY-3.LISP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#Comment

(foobar 'a 'variable '(:foo ?var ?var2 :bar Y) 'info)
(foobar 'a 'variable '(:foo ?var ?var2 :plist Y :exist? tr) 'info)
(<find-rule-command> 'a :mapkeys)
(funcall (<find-rule-type-tester> 'hash-table) 'xx)

(testdef (a hash-table) t
  (:enum
   (:foo `(code1 ,this) `(code2 ,this))
   (:bar `(code3 ,this) 'code4)
   (:baz `(baz ,this))
   ))

(symbol-plist 'a)
(getf (symbol-plist t) :foo)
(symbol-plist '+)

(setf (symbol-plist 'a) nil)

(testdef (hash-table t)  ` (hash-table-p ,this)
  (:enum
   (:copy `(make-hash-table 
            :initial-contents (hash-table-alist ,this)
            :test (hash-table-test ,this)
            :size (hash-table-size ,this)
            :rehash-size (hash-table-rehash-size ,this)
            :rehash-threshold (hash-table-rehash-threshold ,this)))
   (:alist `(hash-table-alist ,this))
   (:plist `(hash-table-plist ,this))

   (:count `(hash-table-count ,this))
   )

  (:bool-query
   (:exist? (key) ` (multiple-value-bind (val e) (gethash ,key ,this) 
                      (declare (ignore val))
                      e))
   )

  (:query
   (:mapkeys (f) `(progn (maphash-keys ,f ,this) ,this))
   (:mapvalues (f) `(progn (maphash-values ,f ,this) ,this))
   )
  (:safe-query
   (:get (key) ` (multiple-value-bind (val e) (gethash ,key ,this) 
                   (if e val ,error)))
   )
  )
