#lang racket

; (1 2) 3

;;; (cons
;;;     (cons 3 '(1 2))
;;;     (map (curry cons 1) '((2 3) (3 2)))
;;; )
 
(define (interleave el lst)
    (if (empty? lst)
        (list (list el))
        (cons (cons el lst)
              (map (curry cons (car lst))
                   (interleave el (cdr lst)))
        )
    )
) 

(require racket/trace)

;;; (trace interleave)
;;; (interleave 1 '(2 3))

(define (permutations lst)
    (if (empty? lst)
        (list '())
        (apply append
               (map (curry interleave (car lst))
                    (permutations (cdr lst))
               )
        )
    )
)


(struct node (name left right) #:transparent)

(define bool-tree
    (node 'x1
        (node 'x2
              (node 'x3 1 0)
              (node 'x3 0 1)
        )
        (node 'x2
              (node 'x3 0 0)
              (node 'x3 1 1)
        )
    )
)

(define (evaluate tree vals)
    (match vals
        [(list) tree]
        [(list 0 vs ...) (evaluate (node-left tree) vs)]
        [(list 1 vs ...) (evaluate (node-right tree) vs)]
    )
)

;;; (trace evaluate)
;;; (evaluate bool-tree '(1 0 1))

(define bt1 (node 'x1
                (node 'x2 1 0)
                (node 'x2 0 1)
            )
)

(struct assigment (var val) #:transparent)

(define (satisficing-evaluations tree [ev '()])
    (match tree
        [1 (list (reverse ev))]
        [0 '()]
        [(node val left right)
            (append (satisficing-evaluations left (cons (assigment val 0) ev))
                    (satisficing-evaluations right (cons (assigment val 1) ev))
            )
        ]
    )
)

;;; (satisficing-evaluations bt1)

(define (sublists lst)
    (if (empty? lst)
        (list '())
        (let ([rest (sublists (cdr lst))])
            (append rest
                    (map (curry cons (car lst)) rest)
            )
        )
    )
)

;;; (trace sublists)
;;; (define list1 '(1 2 3))
;;; (sublists list1) 

(struct mtch (winner left right) #:transparent)

(define tour
  (mtch 'F
        (mtch 'D
              (mtch 'A 'A 'B)
              (mtch 'D 'C 'D))
        (mtch 'F
              (mtch 'F 'E 'F)
              (mtch 'G 'G 'H))))

(define (beaten-teams tournament)
    (define (combine-loosers tournament loosers)
        (match (mtch-winner tournament)
            
            [(mtch-winner (mtch-left tournament)) 
                (combine-loosers 
                    (mtch-left tournament) 
                    (cons 
                        loosers 
                        (mtch-winner (mtch-right tournament))
                    )
                )
            ]

            [(mtch-winner (mtch-right tournament)) 
                (combine-loosers
                    (mtch-right tournament)
                    (cons 
                        loosers
                        (mtch-winner (mtch-left tournament))
                    )
                )
            ]

            [_ loosers]
        )
    )

    (combine-loosers tournament '())
)           

(beaten-teams tour)
