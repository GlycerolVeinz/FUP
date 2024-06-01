#lang racket

; list of nodes
(define ns '(1 2 3 4 5 6)) ; listofnodes
; list of edges where each edge contains (start end cost)
(define es '((1 2 0.5) (1 3 1.0) (2 3 2.0) (2 5 1.0) (3 4 4.0) (4 5 1.0)))
; the graph; a list of nodes and edges
(define gr (list ns es))

; some convenience functions
(define (nodes gr) (car gr))
(define (edges gr) (cadr gr))
(define (cost edge) (caddr edge))

(define (cheaper? x y) 
    (< (cadr x) (cadr y))
)

(define (neighbors node graph)
    (define left-neighbors
        (filter 
            (lambda (edge) (= (first edge) node))
            (edges graph)
        )
    )

    (define right-neighbors
        (filter 
            (lambda (edge) (= (second edge) node))
            (edges graph)
        )
    )

    (cons
        (if (empty? left-neighbors) '() (first left-neighbors))
        (if (empty? right-neighbors) '() (second right-neighbors))
    )
)

(map 
    (lambda 
        (node)
        (display node)
        (display 
            (neighbors node gr)
        )
        (newline)
    ) 
    (nodes gr)
)

(define (extend-paths-queue paths graph)
    (define (extend-path path)
        (define (path-cost path)
            (cadr path)
        )

        (define (path-nodes path)
            (car path)
        )

        (define last-node
            (car (reverse (path-nodes path)))
        )
        
        (define not-visited
            (filter 
                (lambda (neighbor)
                    (not (member (cadr neighbor) (path-nodes path)))
                )
                (neighbors last-node graph)
            )
        )

        (map 
            (lambda (neighbor)
                (list 
                    (append (path-nodes path) (list (cadr neighbor))) 
                    (+ (path-cost path) (cost neighbor))
                )
            ) 
            not-visited
        )
    )

    ;;; Extend all paths in the queue
    (car (map extend-path paths))
)

(define (reached-goal? path goal)
    (= (car (reverse (car path))) goal)
)

(define (generate-flights paths graph)
    (define (generate-flights-iter paths all-paths)
        (define new-paths
            (extend-paths-queue paths graph)
        )

        (if (null? new-paths)
            all-paths
            (generate-flights-iter new-paths (append all-paths new-paths))
        )
    )

    (generate-flights-iter paths paths)
)

;;; (generate-flights (list (list (list 3) 0)) gr)

(define (cheap-flight start end graph)
    (define starting-list 
        (list (list (list start) 0))
    )

    (findf
        (lambda (path) (reached-goal? path end))
        (sort (generate-flights starting-list graph) cheaper?)
    )
)

;;; (cheap-flight 2 3 gr)


