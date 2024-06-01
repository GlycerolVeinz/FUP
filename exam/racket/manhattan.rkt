#lang racket

(define points
    '(
        (#\A 1 1)
        (#\B 1 6)
        (#\C 8 3)
        (#\D 3 4)
        (#\E 5 5)
        (#\F 8 9)
    )
)

(define (getPointLetter point)
    (char-downcase (first point))
)

(define (findMaxGridSize points)
    (let* 
        (
            (x (map (lambda (point) (second point)) points))
            (y (map (lambda (point) (third point)) points))
            (max-x (apply max x))
            (max-y (apply max y))
        )
        (list max-x max-y)
    )
)

;;; (findMaxGridSize points)

(define (createGrid width height)
    (let* 
        (
            (grid (make-vector height))
        )
        (for ([i (in-range height)])
            (vector-set! 
                grid i (make-vector width #\x)
            )
        )
        grid
    )
)


(define (grid->string grid)
    (let* 
        (
            (height (vector-length grid))
            (width (vector-length (vector-ref grid 0)))
            (str "")
        )
        (for ([i (in-range height)])
            (for ([j (in-range width)])
                (set! str 
                    (string-append 
                        str 
                        (string (vector-ref (vector-ref grid i) j))
                    )
                )
            )

            (set! str (string-append str "\n"))
        )
        str
    )
)

;;; (display (grid->string (createGrid 10 10)))

(define (getPointCoords point)
    (list (second point) (third point))
)

(define (manhattanDistance coord1 coord2)
    (+ (abs (- (first coord1) (first coord2))) (abs (- (second coord1) (second coord2))))
)

(define (findClosestPoint pointCoord points)
    (let*
        [
            (minDistance 100000000)
            (closestPoint #\x)
        ]
        (for ([point (in-list points)])
            (let* 
                (
                    (pointCoords (getPointCoords point))
                    (distance (manhattanDistance pointCoord pointCoords))
                )
                (cond 
                    [(< distance minDistance)
                        (set! minDistance distance)
                        (set! closestPoint (getPointLetter point))
                    ]
                    [(= distance minDistance)
                        (set! closestPoint #\.)
                    ]
                )
            )
        )

        closestPoint
    )
)

;;; (findClosestPoint '(0 0) points)

(define (fillGrid grid points)
    (let* 
        (
            (height (vector-length grid))
            (width (vector-length (vector-ref grid 0)))
        )
        (for ([i (in-range height)])
            (for ([j (in-range width)])
                (let* 
                    (
                        (pointCoord (list j i))
                        (closestPoint (findClosestPoint pointCoord points))
                    )
                    (vector-set! 
                        (vector-ref grid i) j closestPoint
                    )
                )
            )
        )

        grid
    )
)

;;; (fillGrid (createGrid 10 10) points)

;;; (display 
;;;     (grid->string 
;;;         (fillGrid (createGrid 10 10) points)
;;;     )
;;; )


(define (putPointsOnGrid grid points)
    (for ([point (in-list points)])
        (let* 
            (
                (pointCoords (getPointCoords point))
                (x (first pointCoords))
                (y (second pointCoords))
            )
            (vector-set! 
                (vector-ref grid y) x (first point)
            )
        )
    )

    grid

)

(define (grid points)
    (define gridSize (findMaxGridSize points))
    
    (define startingGrid (createGrid (add1 (first gridSize)) (add1 (second gridSize))))

    (define gridWithManhattan (fillGrid startingGrid points))

    (define gridWithClosestPoints (putPointsOnGrid gridWithManhattan points))

    (grid->string gridWithClosestPoints)
)

(display (grid points))

