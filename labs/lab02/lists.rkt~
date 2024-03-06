#lang racket

(define lst '(a b c d))

(define (reverse lst [acc '()])
    (if (empty? lst) acc
        (reverse (cdr lst) (cons (car lst) acc))))

(require racket/trace)
(trace reverse)
