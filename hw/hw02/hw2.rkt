#lang racket
;;; SVGEN interpreter

(require racket/trace)
(provide execute)


;;; SVG-Tag generators ===============================================================================
(define (interpretCircle x y r style) 
    (format "<circle cx=\"~a\" cy=\"~a\" r=\"~a\" style=\"~a\" />" x y r style)
)

(define (interpretLine x1 y1 x2 y2 style) 
    (format "<line x1=\"~a\" y1=\"~a\" x2=\"~a\" y2=\"~a\" style=\"~a\" />" x1 y1 x2 y2 style)
)

(define (interpretRect x y width height style) 
    (format "<rect x=\"~a\" y=\"~a\" width=\"~a\" height=\"~a\" style=\"~a\" />" x y width height style)
)

;;; Enviroment =======================================================================================
(define empty-env '())

(define (extend-env key value env)
    (cons (cons key value) env)
)

(define (lookup key env)
    (cond
        [(null? env) (error "No such key in enviroment | key: " key)]
        [(eq? (caar env) key) (cdar env)]
        [else (lookup key (cdr env))]
    )
)

(define (env-init definitions)
    (define (process-definition def env)
        (cond
            [(eq? (car def) 'define) 
                (let 
                    [
                        (name (cadr def))
                        (value (caddr def))
                    ]
                        
                    (if (list? name) ; If it's a function definition
                        (extend-env (car name) (cons (cdr name) (cddr def)) env)
                        ;;; (extend-env (car name) (lambda (cdr name) (eval-expression (cddr def) env)) env)
                        (extend-env name value env)
                    )
                )
            ]
            [else (error "Invalid definition" def)]
        )
    )
    
    (foldl process-definition empty-env definitions)
)

;;; Program evaluation ===============================================================================
(define (eval-expression expr env)
    (cond
        [(number? expr) expr]
        [(string? expr) expr]
        [(symbol? expr) (lookup expr env)]
        [(list? expr) (eval-operation expr env)]
        [(null? expr) '()]
        [else (error "Unknown expression type" expr)]
    )
)

(define (eval-svg-primitive op args env)
    (let
        [
            (evaluated-args (map (lambda (arg) (eval-expression arg env)) args))
        ]
        (case op
            ((circle) (apply interpretCircle evaluated-args))
            ((line) (apply interpretLine evaluated-args))
            ((rect) (apply interpretRect evaluated-args))
        )
    )
)

(define (eval-arithmetic op args env)
    (let
        [
            (evaluated-args (map (lambda (arg) (eval-expression arg env)) args))
        ]
        (case op
            ((+) (apply + evaluated-args))
            ((-) (apply - evaluated-args))
            ((*) (apply * evaluated-args))
            ((/) (apply / evaluated-args))
            ((floor) (apply floor evaluated-args))
            ((cos) (apply cos evaluated-args))
            ((sin) (apply sin evaluated-args))
        )
    )    
)

(define (eval-boolean op args env)
    (let
        [
            (evaluated-args (map (lambda (arg) (eval-expression arg env)) args))
        ]
        (case op
            ((=) (apply = evaluated-args))
            ((<) (apply < evaluated-args))
            ((>) (apply > evaluated-args))
        )
    )    
)

(define (eval-if args env)
    (let
        [
            (condition (first args))
            (then-expr (second args))
            (else-expr (third args))
        ]
        (if (eval-expression condition)
            (eval-expression then-expr env)
            (eval-expression else-expr env)
        )
    )
)

(define (eval-when args env)
    (let
        [
            (condition (eval-expression (car args) env))
            (expressions (cdr args))
        ]
        (when condition
            (for-each (lambda (expr) (eval-expression expr env)) expressions)
        )
    )
)

(define (eval-function-call op args env)
    (let
        [
            (bodies (cdr (lookup op env)))
            (new-env (foldl (lambda (arg value) (extend-env arg value env)) env args))
        ]
        (
            (map (lambda (body) (eval-expression body env)) bodies)
        )
    )
)

(define (eval-operation expr env)
    (let 
        [
            (op (car expr))
            (args (cdr expr))
        ]

        (cond
            ;;; SVG primitives
            ((member op '(circle line rect)) (eval-svg-primitive op args env))
            ;;; Arithmetic
            ((member op '(+ - * / floor cos sin)) (eval-arithmetic op args env))
            ;;; Boolean
            ((member op '(= < >)) (eval-boolean op args env))
            ;;; Conditional
            ((eq? op 'if) (eval-if args env))
            ((eq? op 'when) (eval-when args env))
            ;;; Function call
            [else (eval-function-call op args env)]
            ;;; [else (error "Unknown operation" op)]
        )
    )    
)


;;; Execute ===========================================================================================
(define (execute width height prg expr)    
    (let 
        [
            (enviroment (env-init prg))
        ]
        (let
            [
                (svg-string (eval-expression expr enviroment))
            ]
            (string-append
                
                (format "<svg width=\"~a\" height=\"~a\">" width height) 
                svg-string 
                "</svg>"
            )
        )
        ;;; (for-each (lambda (e) (display e)) enviroment)
    )
)


(define test1
    '((define (start)
        (rect 0 0 100 100 "fill:red")
        (rect 100 0 100 100 "fill:green")
        (rect 200 0 100 100 "fill:blue"))))

(display (execute 400 400 test1 '(start)))


