#lang racket

(require racket/trace)
(require 2htdp/image)

;;; (provide 
;;;     img->mat
;;;     ascii-art)


(define chars " .,:;ox%#@")

(define (intensity->charIndex intensity)
    (floor (/ 
                (*
                    (length chars)
                    (- 
                        255 
                        (floor intensity)
                    )
                )
                256
            ))
)

(define (RGB->grayscale color)
  (+ (* 0.3 (color-red color))
     (* 0.59 (color-green color))
     (* 0.11 (color-blue color))))

;;; Transforms image in to matrix of grayscale values
(define (img->mat img)

    ;;; makes a matrix from a list, matrix width is given as a parameter
    (define (list->mat lst width)
            (cond
                [(empty? lst) empty]
                [(< (length lst) width) (list lst)]
                [else
                    (cons (take lst width) (list->mat (drop lst width) width))
                ]
            )
    )

    (list->mat 
        (map RGB->grayscale (image->color-list img)) 
        (image-width img)
    )
)


(define (ascii-art width height chars)
    (lambda (img-path)
        (let* 
            ([row 0]
            [coll 0])
            
        )
    )


)

