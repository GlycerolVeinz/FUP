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
     (* 0.11 (color-blue color)))
)


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
(define (intensity->charIndex intensity chars)
  (inexact->exact (floor (/ (* (string-length chars) (- 255 (floor intensity))) 256)))
)

(define (summAndAverageMatrix mat width height)
  (define (averageBlock block)
    (/ (apply + (apply append block)) (* width height))
  )

  (define (flatmapColsToRows mat row width height)
    (map 
      (lambda 
        (row) 
        (list->mat row width discardExcess)) 
      (take mat height)
    )
  )

  (if (< (length mat) height)
    '()
    (cons 
      (apply 
        map 
        (lambda selectedBlock 
            (averageBlock selectedBlock))
            (flatmapColsToRows mat height width height))
      (summAndAverageMatrix (drop mat height) width height)
    )
  )
)

(define (matrix->string mat chars)
  (apply 
    string-append 
    (map 
      (lambda (row) 
        (string-append 
          (apply 
            string-append 
            (map 
              (lambda (intensity) 
                (string (string-ref chars (intensity->charIndex intensity chars))))
              row))
          (string #\newline)))
      mat)))

(define (ascii-art width height chars)
  (lambda (img) (matrix->string (summAndAverageMatrix (img->mat img) width height) chars))
)

;;; Testing ==========================================================================================
;;; (define ch " .,:;ox%#@")

;;; (define example 
;;;     (above
;;;         (beside (rectangle 2 1 "solid" (make-color 0 0 0))
;;;                 (rectangle 3 1 "solid" (make-color 75 75 75)))
;;;         (beside (rectangle 2 3 "solid" (make-color 180 180 180))
;;;                 (rectangle 3 3 "solid" (make-color 225 225 225)))))


;;; (display ((ascii-art 2 2 ch) example))
