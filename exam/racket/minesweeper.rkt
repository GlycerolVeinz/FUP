#lang racket

; for testing
(define test-board
  (map string->list (string-split ".*..\n..*.\n**..\n...*\n*...")))

; for converting ints to chars.
(define (int->digit i) (integer->char (+ 48 i)))

(define (getNeighbours field width height x y)
    (for*/list (
            (i (range (max 0 (- y 1)) (min height (+ y 2))))
	        (j (range (max 0 (- x 1)) (min width (+ x 2))))
        )
        (list-ref (list-ref field i) j)
    )
)

(define (countMines neighbours)
    (define isMine? 
        (lambda (c) (char=? c #\*))
    )

    (count isMine? neighbours)
)

(define (displayField field)
    (define height (length field))
    (define width (length (list-ref field 0)))

    (define (displayTile tile x y)
        (match tile
            (#\* (display "*"))
            (#\. (display (countMines (getNeighbours field width height x y))))
            (else (display (int->digit tile)))
        )
    )

    (for* ((y (range 0 height))
           (x (range 0 width)))
        (displayTile 
            (list-ref (list-ref field y) x) 
            x y
        )
        (if (= x (- width 1)) 
            (newline) 
            (display "")
        )
    )
)

(display test-board)
(newline)
(displayField test-board)

;;; (let* 
;;;     (
;;;         (input-string (port->lines))
;;;         ; implement parsing of board/sweep for mines

;;;         ; assuming counted-board contins a list of list of chars
;;;         (sn (map list->string counted-board))
;;;     )
    
;;;     (for ((l sn))
;;;         (display l)
;;;         (newline)
;;;     )
;;; )