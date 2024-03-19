#lang racket

;;; Header ===========================================================================================

(require racket/trace)
(require 2htdp/image)

(provide 
    img->mat
    ascii-art
)

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
            (cons (take lst size) (list->mat (drop lst size) size excessHandler))
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
    (define (img->blockMat img blockWidth blockHeight)
        (define (averageBlockMat blockMat)
            (map 
                (lambda (row)
                    (map 
                        (lambda (block)
                            (floor (/ (apply + block) (length block)))
                        )
                        row
                    )
                )
                blockMat
            )
        )
        
        (averageBlockMat 
            (list->mat 
                (list->mat 
                    (img->mat img) 
                    blockWidth
                    discardExcess
                ) 
                blockHeight
                discardExcess
            )
        )
    )

    (define (blockMat->string blockMat)
        (foldr 
            (lambda (row acc)
                (string-append 
                    (foldr 
                        (lambda (intensity acc)
                            (string-append acc (string (list-ref chars (intensity->charIndex intensity))))
                        )
                        "" 
                        row
                    )
                    "\n"
                    acc
                )
            )
            "" 
            blockMat
        )
    )

    (define (averagedBlockMat->intensitiesBlockMat blockMat)
        (map 
            (lambda (row)
                (map 
                    (lambda (intensity)
                        (list-ref chars (intensity->charIndex intensity))
                    )
                    row
                )
            )
            blockMat
        )
    )

    (lambda (img)
        (blockMat->string 
            (averagedBlockMat->intensitiesBlockMat 
                (img->blockMat img width height)
            )
        )
    )
)

(define example 
    (above
        (beside (rectangle 2 1 "solid" (make-color 0 0 0))
                (rectangle 3 1 "solid" (make-color 75 75 75)))
        (beside (rectangle 2 3 "solid" (make-color 180 180 180))
                (rectangle 3 3 "solid" (make-color 225 225 225)))))

((ascii-art 2 2 chars) example)
