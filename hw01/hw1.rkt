#lang racket

;;; (require 2htdp/image)
;;; (provide 
;;;     img->mat
;;;     ascii-art)


;;; (define chars ".,;'/\")

(define (asciiNum c) 
    (char->integer c))



(define (readUntilEof) 
    (let ([inputCh (read-char)])
       (cond 
            [(eof-object? inputCh) 
                (newline)
                (display "End")
                (newline)
                ]
            [else
                (write (asciiNum inputCh)) 
                (readUntilEof)]
        )
    )
)

;;; (readUntilEof)

(readUntilEof)

