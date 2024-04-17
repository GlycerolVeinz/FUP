#lang racket

(require racket/trace)
(provide execute)

;; Functions generating SVG-tags
(define (generate-circle x y r style)
  (format "<circle cx=\"~a\" cy=\"~a\" r=\"~a\" style=\"~a\"/>" x y r style))

(define (generate-rect x y width height style)
  (format "<rect x=\"~a\" y=\"~a\" width=\"~a\" height=\"~a\" style=\"~a\"/>" x y width height style))

(define (generate-line x1 y1 x2 y2 style)
  (format "<line x1=\"~a\" y1=\"~a\" x2=\"~a\" y2=\"~a\" style=\"~a\"/>" x1 y1 x2 y2 style))


;; Environment functions =============================================================================
(define (create-initial-environment definitions)
    (define (process-definition def env)
        (cond
            ((eq? (car def) 'define) ; Check if it's a definition
            (let ((name (cadr def))
                    (value (caddr def)))
                    
                (if (list? name) ; If it's a function definition
                    (cons (cons (car name) (cons (cdr name) (cddr def))) env)
                    (cons (list name value) env))))
            (else (error "Invalid definition" def))
        )
    )
    
    (foldl process-definition '() definitions)
)

;; Evaluator functions ===============================================================================
(define (evaluate-expression expr env acc)
    (cond
        ((symbol? expr) (lookup expr env))
        ((number? expr) expr)
        ((string? expr) expr)
        ((list? expr) (evaluate-list expr env acc))
        (else (error "Invalid expression" expr))
    )
)

(define (evaluate-list expr env acc)
    (let ((operator (car expr))
            (arguments (cdr expr)))
        (case operator
            ((+) (apply + (map (lambda (arg) (evaluate-expression arg env acc)) arguments)))
            ((-) (apply - (map (lambda (arg) (evaluate-expression arg env acc)) arguments)))
            ((*) (apply * (map (lambda (arg) (evaluate-expression arg env acc)) arguments)))
            ((/) (apply / (map (lambda (arg) (evaluate-expression arg env acc)) arguments)))
            ((floor) (floor (evaluate-expression (car arguments) env acc)))
            ((cos) (cos (evaluate-expression (car arguments) env)))
            ((sin) (sin (evaluate-expression (car arguments) env)))
            ((=) (apply equal? (map (lambda (arg) (evaluate-expression arg env acc)) arguments)))
            ((<) (apply < (map (lambda (arg) (evaluate-expression arg env acc)) arguments)))
            ((>) (apply > (map (lambda (arg) (evaluate-expression arg env acc)) arguments)))
            (else (evaluate-application expr env acc))
        )
    )
)

(define (lookup id env)
    (cond
        ((symbol? id) 
            (let ((binding (assq id env)))
                (if binding
                    (let ((value (cdr binding)))
                        (if (list? value)
                            (apply string-append value)
                            value)
                    )
                    (error "Unbound variable" id))
            )
        )
        (else id)
    )
)

(define (evaluate-application expr env acc)
    (let ((operator (car expr))
        (arguments (map (lambda (arg) (evaluate-expression arg env acc)) (cdr expr))))
        (case operator
            ((circle) (string-append acc (apply generate-circle arguments)))
            ((rect) (string-append acc (apply generate-rect arguments)))
            ((line) (string-append acc (apply generate-line arguments)))
            (else (error "Unknown operator" operator))
        )
    )
)


;;; Execute function =================================================================================
(define (execute width height prg expr)
    (define env (create-initial-environment prg))
    (let ((result (evaluate-expression expr env "")))
        (format "<svg width=\"~a\" height=\"~a\">~a</svg>" width height result)
    )
)

;;; Test =============================================================================================
(define test1
    '(
        (define (start)
        (rect 0 0 100 100 "fill:red")
        (rect 100 0 100 100 "fill:green")
        (rect 200 0 100 100 "fill:blue"))
    )
)


(trace create-initial-environment)

(display (execute 400 400 
             '((define STYLE "fill:red"))
             '(circle 200 200 (floor (/ 200 3)) STYLE)))
