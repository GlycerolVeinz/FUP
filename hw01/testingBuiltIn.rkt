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