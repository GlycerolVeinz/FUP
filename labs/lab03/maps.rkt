#lang racket

(define l1 '(1 2 3))
(define l2 '(1 -2))

;;; (apply append
;;;     (map 
;;;         (lambda 
;;;             (e1) 
;;;             (map 
;;;                 (curry * e1) 
;;;                 l2)
;;;         ) 
;;;         l1
;;;     )
;;; )

(define (f-all-pair func l1 l2)
    (apply append
        (map 
            (lambda 
                (e1)
                (map
                    (curry 
                        func 
                        e1
                    )
                    l2
                )
            )
            l1 
        )
    )
)

;;; (f-all-pair cons l1 l2)

(define p1 '((1 0) (1 1)))
(define p2 '((-1 0) (1 1) (3 2)))

(define (getCoef m) (car m))
(define (getExp m) (cadr m))

(define (addMon m1 m2)
    (list 
        (+ 
            (getCoef m1) 
            (getCoef m2)
        )

        (getExp m1)   
    )
)

(define (multMon m1 m2)
    (list 
        (* (getCoef m1) (getCoef m2))
        (+ (getExp m1) (getExp m2))
    )
)

(define (addMonPol mon pol)
    (define (sameExp? m) 
        (=
            (getExp m)
            (getExp mon)    
        )
    )

    (define sameMon
        (filter
            sameExp?
            pol
        )
    )

    (define rest
        (filter
            (compose not sameExp?)
            pol
        )
    )

    (if (empty? sameMon)
        (cons mon rest)
        (cons (addMon mon (car sameMon)) rest)
    )
)

(define (normalize p)
    (define (nonZeroCoef? m) (not (= 0 (getCoef m))))
    (sort
        (filter nonZeroCoef? p)
        (lambda (m1 m2) (< (getExp m1) (getExp m2)))
    )
)

(define (simplifyPol pol)
    (foldl addMonPol '() pol)
)

;;; (normalize (addMonPol '(3 2) p2))

(define (multMonPol p1 p2)
    (normalize (simplifyPol (f-all-pair multMon p1 p2)))
)

;;; (multMonPol p1 p2)

(define (multiplyBySkalar vecList skalar)
    (map (curry * skalar) vecList)
)

(define (linearCombination vectorsList scalarsList)
    (foldl 
        (lambda (v1 v2) (map + v1 v2))
        (multiplyBySkalar (car vectorsList) (car scalarsList))
        (map 
            (lambda (v s) (multiplyBySkalar v s))
            (cdr vectorsList)
            (cdr scalarsList)
        )
    )
)



(define v1 '((1 2 3) (1 0 1) (0 2 0)))
(define l3 '(2 -1 3))
(linearCombination v1 l3)






