#lang racket

(define (getPoint board coord)
    (let 
        (
            [x (car coord)]
            [y (cadr coord)]
        )
        (list-ref (list-ref board x) y)
    )
)

(define knight-moves
    (list
        (list 2 1)
        (list 2 -1)
        (list 1 2)
        (list 1 -2)
        (list -1 2)
        (list -1 -2)
        (list -2 1)
        (list -2 -1)
    )
)

(define (find-knights board)
    (let
        (
            [size (length board)]
            [knights '()]
        )
        (for ([x (in-range size)])
            (for ([y (in-range size)])
                (if (= (getPoint board (list x y)) '1)
                    (set! knights (cons (list x y) knights))
                    (void)
                )
            )
        )
        knights
    )
)

(define (inBounds? x y size)
    (and (>= x 0) (< x size) (>= y 0) (< y size))
)

(define (generate-all-knights-moves board)
    (let
        (
            [size (length board)]
            [knights (find-knights board)]
            [moves '()]
        )
        (for ([knight (in-list knights)])
            (for ([move (in-list knight-moves)])
                (let
                    (
                        [x (+ (first knight) (first move))]
                        [y (+ (second knight) (second move))]
                    )
                    (if (inBounds? x y size)
                        (set! moves (cons (list x y) moves))
                        (void)
                    )
                )
            )
        )
        moves
    )
)


(define (coordEqual? coord1 coord2)
    (and 
        (=
            (first coord1) 
            (first coord2)
        )
        (=
            (second coord1)
            (second coord2)
        )
    )
)

(define (is_valid? board)
    (let
        (
            [knights (find-knights board)]
            [moves (generate-all-knights-moves board)]
            [res #t]
        )
        (for ([knight (in-list knights)])
            (for ([move (in-list moves)])
                (if (coordEqual? knight move)
                    (set! res #f)
                    (void)
                )                
            )
        )
        res
    )
)

(is_valid? 
    '(
        (1 0 0 0)
        (0 0 0 1)
        (1 0 0 0)
        (0 1 0 0)
    )
)

(is_valid? '
    (
        (0 1 0 0)
        (0 0 0 1)
        (1 0 0 0)
        (0 0 1 0)
    )
)

