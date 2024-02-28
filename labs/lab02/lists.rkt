#lang racket

(define 
  (reverse
    lst
    [acc '()]
  )
    
  (if (empty? lst)
      acc

      (reverse
          (cdr lst)
          (cons
            (car lst)
            acc
          )
      )
  )
)

(define (group-same lst)
  (define (iter l gr)
    (cond
      [(null? l) (list gr)]
      [(eqv? (car gr) (car l)) (iter (cdr l) (cons (car gr) gr))]
      [else (cons gr (iter (cdr l) (list (car l))))]))
  (if (null? lst)
      '()
      (iter (cdr lst) (list (car lst)))))


(define (average-list lst)
  
  (define (sum-list lst partilSumm)
    
    (if (empty? lst)
      partilSumm
   
      ;;; else
      (sum-list 
        (cdr lst) 
        (+ partilSumm (car lst))) 
    )
  )

  (/ (sum-list lst 0) (length lst))
)


(define (myAverage lst)
  
  (define (average lst summ count)
    (if (empty? lst)
      (/ summ count)

      (average
        (rest lst)
        (+ summ (car lst))
        (+ count 1)
      )
    )
  )

  (average lst 0 0)
)

(define (splitList n lst)
  (if (empty? lst)
      '()
      
      (cons
        (take lst n)
        (splitList 
          n 
          (drop lst n))
      )
  )
)



(define lst '(a b c d))

(define numbers '(1 2 3))

(define longList '(a d g 2 4 1 a b))

(require racket/trace)
(trace reverse)
(trace average-list)
(trace myAverage)
(trace splitList)

(splitList 3 longList)
