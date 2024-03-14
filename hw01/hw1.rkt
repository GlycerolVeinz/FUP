#lang racket

;;; Header ===========================================================================================

(require racket/trace)
(require 2htdp/image)

;;; (provide 
;;;     img->mat
;;;     ascii-art)

;;; First function part ==============================================================================

(define (RGB->grayscale color)
  (+ (* 0.3 (color-red color))
     (* 0.59 (color-green color))
     (* 0.11 (color-blue color))))


(define (discardExcess lst) empty)
(define (wrapExcess lst) (list lst))

;;; makes a matrix from a list, matrix width is given as a parameter, drops excess elements
(define (list->mat lst size excessHandler)
    (cond
        [(empty? lst) empty]
        [(< (length lst) size) (excessHandler lst)]
        [else
            (cons (take lst size) (list->mat (drop lst size) size))
        ]
    )
)

;;; Transforms image in to matrix of grayscale values
(define (img->mat img)
    (list->mat 
        (map RGB->grayscale (image->color-list img)) 
        (image-width img)
        discardExcess
    )
)


;;; Second function part =============================================================================

(define chars " .,:;ox%#@")

(define (intensity->charIndex intensity)
    (floor (/ (* (length chars) (- 255 (floor intensity))) 256))
)

(define (ascii-art width height chars)
    (define (img->blockMat img width height)
        (list->mat 
            (list->mat 
                (img->mat img) 
                width 
                discardExcess
            ) 
            height
            discardExcess
        )
    )

    
)
