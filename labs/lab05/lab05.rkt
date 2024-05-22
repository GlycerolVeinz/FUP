#lang racket

(define (stream-add s1 s2)
    (stream-cons (+ (stream-first s1) (stream-first s2))
        (stream-add (stream-rest s1) (stream-rest s2))
    )
)

(define (ints-from n)
    (stream-cons n (ints-from (+ n 1)))
)

(define (fibonacci-stream)
    (stream* 0 1 
        (stream-add fibs (stream-rest fibs))
    )
)

(struct graph (nodes edges)))

(define ((edge? gr) pair)
    (define edges (graph-edges gr)
        (or
            (member pair edges)
            (member (reverse pair) edges)
        )
    )
)

