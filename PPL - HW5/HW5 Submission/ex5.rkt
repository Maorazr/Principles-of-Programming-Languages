#lang racket

(require rackunit)
(require math/base)

(provide (all-defined-out))

(define id (lambda (x) x))
(define cons-lzl cons)
(define empty-lzl? empty?)
(define empty-lzl '())
(define head car)
(define tail
  (lambda (lzl)
    ((cdr lzl))))

(define take
  (lambda (lz-lst n)
    (if (or (= n 0) (empty-lzl? lz-lst))
      empty-lzl
      (cons (head lz-lst)
                 (take (tail lz-lst) (- n 1))))))
(define integers-from
  (lambda (n)
    (cons-lzl n (lambda () (integers-from (+ n 1))))))


(define plus$
 (lambda (x y cont) (cont (+ x y))))
(define div$
  (lambda (x y cont) (cont (/ x y))))
(define square$
  (lambda (x cont) (cont(* x x))))
(define add1$
  (lambda (x cont) (cont(+ x 1))))
(define div2$
  (lambda (x cont) (cont(/ x 2))))
(define g-0$
  (lambda (n cont)
    (cont (if (> n 0) #t #f))))
(define bool-num$
  (lambda (b cont)
    (cont (if b 1 0))))


;;; Q1.a
; Signature: compose(f g)
; Type: [T1 -> T2] * [T2 -> T3]  -> [T1->T3]
; Purpose: given two unary functions return their composition, in the same order left to right
; test: ((compose - sqrt) 16) ==> -4
;       ((compose not not) true)==> true
(define compose
  (lambda (f g)
    (lambda (x)
       (g (f x)))))

 ;  (cont1 (lambda (x cont2)
 ;           (cont2 (g (f x)))))

;(compose$ f$ g$ c) = (c (compose f g))
;(cont1 (lambda (x cont2)
;            (cont2 (g (f x)))))
;===
;(cont1 (lambda (x)
;       (g (f x))))

; final
(define compose$
  (lambda (f$ g$ cont1)
    (cont1 (lambda (x cont2)
          (f$ x
              (lambda (f-res)
                (g$ f-res cont2)))))))



; Signature: pipe(lst-fun)
; Type: [[T1 -> T2],[T2 -> T3]...[Tn-1 -> Tn]]  -> [T1->Tn]
; Purpose: Returns the composition of a given list of unary functions. For (pipe (list f1 f2 ... fn)), returns the composition fn(....(f1(x)))
; test: ((pipe (list sqrt - - number?)) 16)) ==> true
;       ((pipe (list sqrt - - number? not)) 16) ==> false
;       ((pipe (list sqrt add1 - )) 100) ==> -11
(define pipe
  (lambda (fs)  
    (if (empty? (cdr fs))
        (car fs)
        (compose (car fs) (pipe (cdr fs))))))


; Signature: pipe$(lst-fun,cont)
;         [T1 * [T2->T3] ] -> T3,
;         [T3 * [T4 -> T5] ] -> T5,
;         ...,
;         [T2n-1 * [T2n * T2n+1]] -> T2n+1
;        ]
;        *
;       [[T1 * [T2n -> T2n+1]] -> T2n+1] -> 
;              [[T1 * [T2n+1 -> T2n+2]] -> T2n+2]
;      -> [T1 * [T2n+3 -> T2n+4]] -> T2n+4
; Purpose: Returns the composition of a given list of unary CPS functions.
(define pipe$
  (lambda (lst-fun cont)
    (if (empty? (cdr lst-fun))
        (cont (car lst-fun))
           (pipe$ (cdr lst-fun)
               (lambda (res-pipe)
                 (compose$ (car lst-fun) res-pipe cont))))))


                  
; Signature: reduce-prim$(reducer, init, lst, cont)
; Type: [ [ [T*T -> T] * T * List[T] * [T-> T] ] -> T ]
; Purpose: Returns the reduced value of the given list, from left 
;          to right, with cont post-processing
; Pre-condition: reducer is primitive
; test: (reduce-prim$ + 0 '( 8 2 2) (lambda (x) x))==> 15
;      (reduce-prim$ * 1 '(1 2 3 4 5) (lambda (x) x)) ==> 120
;      (reduce-prim$ - 1 '(1 2 3 4 5) (lambda (x) x))==> -14
(define reduce-prim$
  (lambda (reducer init lst cont)
    (if (empty? lst)
        (cont init)
       (reduce-prim$ reducer
                 init
                 (cdr lst)
                   (lambda (cdr-res)
                     (cont (reducer cdr-res (car lst))))))))
    

     
; Signature: reduce-user$(reducer, init, lst, cont)
; Type: [ [ [T1*T2*[T2 -> T3]-> T2] * T2 * List[T2] * [T2 -> T3] ] -> T2 ]
; Purpose: Returns the reduced value of the given list, from left 
;          to right, with cont post-processing
; Pre-condition: reducer is a CPS user prococedure
; test: (reduce-user$ plus$ 0 '(3 8 2 2) (lambda (x) x)) ==> 15
;        (reduce-user$ div$ 100 '(5 4 1) (lambda (x) (* x 2))) ==> -14
(define reduce-user$
  (lambda (reducer$ init lst cont)
    (if (empty? lst)
        (cont init)
        (reduce-user$ reducer$ init (cdr lst) 
         (lambda (cdr-res)
           (reducer$ cdr-res (car lst) cont))))))


;;; Q2.c.1
; Signature: take1(lz-lst,pred)
; Type: [LzL<T>*[T -> boolean] -> List<T>]
; Purpose: while pred holds return the list elments
; Tests: (take-while (integers-from 0) (lambda (x) (< x 9)))==>'(0 1 2 3 4 5 6 7 8)
;          (take-while(integers-from 0) (lambda (x)  (= x 128))))==>'()
(define take-while
  (lambda (lzl pred)
    (if (or (empty-lzl? lzl) (not (pred (head lzl))))
        empty-lzl
        (cons (head lzl)
              (take-while (tail lzl) pred)))))

                             
         
;;; Q2.c.2
; Signature: take-while-lzl(lz-lst,pred)
; Type: [LzL<T>*[T -> boolean] -> Lzl<T>]
; Purpose: while pred holds return list elments as a lazy list
; Tests: (take (take-while-lzl (integers-from 0) (lambda (x) (< x 9))) 10) ==>'(0 1 2 3 4 5 6 7 8)
;           (take-while-lzl(integers-from 0) (lambda (x)  (= x 128))))==>'()
(define take-while-lzl
  (lambda (lzl pred)
    (if (or (empty-lzl? lzl) (not (pred (head lzl))))
        empty-lzl
        (cons-lzl (head lzl) (lambda () (take-while-lzl (tail lzl) pred))))))
    


;;; Q2.d
; Signature: reduce-lzl(reducer, init, lzl)
; Type: @TODO
; Purpose: Returns the reduced value of the given lazy list
(define reduce-lzl (lambda (reducer init lzl)
    (if (empty-lzl? lzl)
        init
        (reduce-lzl reducer
                 (reducer init (head lzl))
                 (tail lzl)))))




