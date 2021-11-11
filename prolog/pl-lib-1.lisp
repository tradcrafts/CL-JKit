;; -*- coding: utf-8 -*-
;; This file is part of CL-JKit.
;; Copyright (c) 2012-2021 tradcrafts

(jkit.base:jkit-base-header)
(in-package :jkit.prolog)


(pl. loop (:-))
(pl-circulate 'loop)

@export*
(pl. foreach 
     (?src ?x :- (:nonvar ?src) (:is/2 ?itor (itor-begin ?src)) (:undef ?src) ($foreach ?itor ?x)))
(pl. $foreach
     (?itor ? :- (:lop (itor-end-p ?itor)) (:cut) (:fail))
     (?itor ?x :- (:is/2 ?x (itor-get ?itor)) (:lisp (itor-incr ?itor))))
(pl-circulate '$foreach)

@export*
(pl. append
     (?a ?b ?c :- (:nonvar ?c) (:cut) ($append ?a ?b ?c))
     (?a ?b ?c :- (:nonvar ?a ?b) (:cut) ($append ?a ?b ?c)))

(pl. $append
     ((?D . ?x) ?b (?D . ?y) :- ($append ?x ?b ?y))
     (nil ?x ?x))

@export*
(pl. not
     (?x :- ?x (:cut) (:fail))
     (?x))

@export*
(pl. /= (?x ?y :- (not (:= ?x ?y))))

(define-pl-op  boundp (0 v) (boundp v))
@export
(define-pl-op* set! (0 v a) (setf (symbol-value v) a))
(define-pl-op* makunbound (0 v) (makunbound v))
(define-pl-op  symbol-value (1 v) (symbol-value v))
(define-pl-op  gensym (1) (gensym))
(define-pl-op* push (0 a v) (push a (symbol-value v)))
(define-pl-op* pop (1 v) (pop (symbol-value v)))
@export
(define-pl-op* apply! (1 v f) (setf (symbol-value v)
                                      (funcall f (symbol-value v))))

(define-pl-op atom (0 a)  (atom a))
(define-pl-op listp (0 a) (listp a))
(define-pl-op consp (0 a) (consp a))
@export
(define-pl-op varp (0 a) (and (consp a)
                              (eql (car a) :*VAR*)))

(define-pl-op < (0 a b) (< a b))
(define-pl-op <= (0 a b) (<= a b))
(define-pl-op > (0 a b) (> a b))
(define-pl-op >= (0 a b) (>= a b))

(define-pl-op print (0 x) (print x) t)
(define-pl-op 1+ (1 x) (1+ x))
(define-pl-op 1- (1 x) (1- x))
(define-pl-op car (1 xs) (car xs))
(define-pl-op cdr (1 xs) (cdr xs))
(define-pl-op length (1 xs) (length xs))
(define-pl-op nth (1 n xs) (nth n xs))
(define-pl-op nthcdr (1 n xs) (nthcdr n xs))
(define-pl-op cons (1 a b) (cons a b))

@export*
(pl. <- (?dst ?src :- (:is ?dst (identity ?src))))

(define-pl-macro term (&rest terms)
  (let ((v (pl-genvar)))
    `(every ((:is ,v (identity ,terms))
             ,v))))

@export*
(pl. success ())
@export*
(pl. failure (:- (fail)))
(pl. null (nil))
@export*
(pl. head ((?x . ?) ?x :- (:nonvar ?x)))
@export*
(pl. tail ((? . ?x) ?x :- (:nonvar ?x)))

@export*
(pl. or/2  
     (?a ?b :- ?a (:cut))
     (?a ?b :- ?b))

@export*
(pl. try  
     (?a ?b :- ?a (:cut))
     (?a ?b :- ?b (:fail)))

@export*
(pl. and/2 (?a ?b :- ?a ?b))
@export*
(pl. bind (?dst ?x :- (:is ?dst (identity ?x))))

@export*
(pl.  every
      (nil)
      ((?x . ?xs) :- ?x (every ?xs)))

(pl. and 
     (nil :-)
     ((?x . ?xs) :- ?x (every ?xs)))

@export*
(pl. some
     (nil :- (failure))
     ((?x . ?xs) :- ?x (:cut))
     ((?x . ?xs) :- (some ?xs)))

(pl. or
     (nil :- (failure))
     ((?x . ?xs) :- ?x (:cut))
     ((?x . ?xs) :- (some ?xs)))

@export*
(pl. map
     (?f (?x . ?xs) ((?f ?x) . ?ys)  :- (map ?f ?xs ?ys))
     (?f nil nil))

(pl. member
     (?a ?b :- (:nonvar ?b) ($member ?a ?b)))

(pl. $member
     (?a (?a . ?))
     (?a (? . ?xs) :- ($member ?a ?xs)))

@export*
(pl. add
     (?a ?b ?c :- (:any-var ?a ?c) ($add ?a ?b ?c)))

(pl. $add
     ((?x . ?xs) ?a (?x . ?ys) :- ($add ?xs ?a ?ys))
     (nil ?a (?a)))

@export*
(pl. list?
     (?x :- (:var ?x) (:cut) (:fail))
     ((?x . ?xs))
     (nil))

@export*
(pl. symbol?
     (?x :- (:lop (symbolp ?x))))

(pl. intern 
     (?name ?result :- (:is ?result (intern ?name :pl)))
     (?name ?package ?result :- (:is ?result (intern ?name ?package))))

(pl. reverse 
     (?x ?y :- (:nonvar ?x) (:cut) ($reverse ?x nil ?y))
     (?x ?y :- (:nonvar ?y) ($reverse ?y nil ?x)))

(pl. $reverse
     (nil ?l ?l)
     ((?x . ?l) ?y ?r :- ($reverse ?l (?x . ?y) ?r)))

@export*
(pl. permutation
     (?a ?b :- (:nonvar ?a) (:cut) ($permutation ?a ?b))
     (?a ?b :- (:nonvar ?b) ($permutation ?b ?a)))

(pl. $permutation
     (nil nil :- (:cut))
     (?l (?x . ?l2) :- (del ?x ?l ?l1) ($permutation ?l1 ?l2)))

@export*
(pl. del
     (?a ?b ?c :- (:nonvar ?b) ($del ?a ?b ?c)))
(pl. $del
     (?x (?x . ?l) ?l)
     (?x (?y . ?l) (?y . ?l1) :- ($del ?x ?l ?l1)))

@export*
(define-pl-macro call (form)
  (let ((tmp (pl-genvar)))
    `(and (:is ,tmp (identity ,form)) ,tmp)  ))
                                        ;(pl. map! 
                                        ;         (?f (?x . ?xs) (?y . ?ys) :- (bind ?e (?f ?x '?y)) ?e (map! ?f ?xs ?ys))
                                        ;         (?f nil nil))


@export*
(pl. findall
     (?x ?term ?dst :- 
         (gensym ?tmpvar) 
         (set! ?tmpvar nil) 
         (findall ?tmpvar ?x ?term ?dst))
     (?tmpvar ?x ?term ?dst :- ?term (push ?x ?tmpvar) (:fail))
     (?tmpvar ?x ?term ?dst :- 
              (apply! ?tmpvar nreverse ?result)
              (:= ?dst ?result)
              (makunbound ?tmpvar)))

(pl. findall
     (?x ?term ?dst :- 
         (gensym ?tmpvar) 
         (set! ?tmpvar nil) 
         ((:strict findall-body) ?tmpvar ?x ?term ?dst)))

(pl. (:strict findall-body)
     (?tmpvar ?x ?term ?dst :- ?term (push ?x ?tmpvar) (:fail))
     (?tmpvar ?x ?term ?dst :- 
              (apply! ?tmpvar nreverse ?result)
              (:= ?dst ?result)
              (makunbound ?tmpvar)))


(define-pl-macro test (v)
  `(:is ,v (identity (,(pl-genvar) ,(pl-genvar)))))
