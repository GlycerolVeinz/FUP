#lang racket

(define (sum a b c)
  (+ a b c))

(define args (list 1 2 3))

(define result (apply sum args))

(displayln result)


;;; (define (readUntilEof) 
;;;     (let ([inputCh (read-char)])
;;;        (cond 
;;;             [(eof-object? inputCh) 
;;;                 (newline)
;;;                 (display "End"")
;;;                 (newline)
;;;                 ]
;;;             [else
;;;                 (write (asciiNum inputCh)) 
;;;                 (readUntilEof)]
;;;         )
;;;     )
;;; )

(define (ascii-art width height chars)
  (lambda (img-path)
    (define img (bitmap img-path)) ; Load the image.
    (define img-gray (convert-to-grayscale img)) ; Convert to grayscale. Implement this accordingly.
    (define (average-intensity block) ... ) ; Function to calculate average intensity of a block.
    
    (define cols ...)
    (define rows ...)
    (define char-length (string-length chars))
    
    (let loop ((x 0) (y 0) (result ""))
      (cond
        [(>= y rows) result]
        [(>= x cols)
         (loop 0 (+ y height) (string-append result "\n"))
        ]
        [else
         (let* ((block (get-block img-gray x y width height)) ; Implement get-block accordingly.
                (avg-int (average-intensity block))
                (index (floor (* (sub1 char-length) avg-int) 255)) ; Index calculation, adjust as necessary.
                (char (string-ref chars index)))
           (loop (+ x width) y (string-append result (string char))))])
    )
  )
)



(define (sum-blocks-heightwise-size-two mat)
  (if (or (empty? mat) (empty? (rest mat)))
      '()
      (cons (map (lambda (block1 block2) 
                   (list (+ (list-ref block1 0) (list-ref block2 0)) 
                         (+ (list-ref block1 1) (list-ref block2 1))))
                 (list->mat (first mat) 2 discardExcess)
                 (list->mat (second mat) 2 discardExcess))
            (sum-blocks-heightwise-size-two (cddr mat)))))

(define (sum-blocks-heightwise mat height)
  (if (< (length mat) height)
      '()
      (let ((blocks (map (lambda (row) (list->mat row 2 discardExcess)) 
                         (take mat height))))
        (cons (apply map 
                     (lambda args 
                       (list (apply + (map first args)) 
                             (apply + (map second args))))
                     blocks)
              (sum-blocks-heightwise (drop mat height) height)))))

(define (sum-blocks-let mat width height)
  (if (< (length mat) height)
      '()
      (let* ((rows (take mat height))
             (blocks (map (lambda (row) (list->mat row width discardExcess)) rows))
             (sums (apply map 
                          (lambda args 
                            (list (/ (apply + (apply append args)) (* width height))))
                          blocks)))
        (cons sums (sum-blocks-let (drop mat height) width height)))))

(display 
    (map 
        (lambda (matrix) 
            (map 
                (lambda (row) 
                    (map
                        (lambda (block) (/ (apply + block) 4))
                        (list->mat row 2 discardExcess)
                    ))
                matrix
            ))
        (sum-blocks-heightwise (img->mat example) 2)
    )
)

(define (sum-blocks mat width height)
  (define (averageBlock block)
    (/ (apply + (apply append block)) (* width height))
  )

  (if (< (length mat) height)
    '()
    (cons 
    (apply map 
              (lambda elements  ; renamed args to elements
                  (averageBlock elements))  ; renamed args to elements
                  (map 
                    (lambda 
                      (row) 
                      (list->mat row width discardExcess)) 
                    (take mat height)))
            (sum-blocks (drop mat height) width height)
    )
  )
)
