#lang racket

; Header =============================================================================================
(require racket/trace)
(provide execute)

(define num-ops  '(+ - * / floor cos sin))
(define bool-ops '(= < >))
(define svg-ops  '(circle line rect))

; Helper functions ===================================================================================
(define (get-op op) 
    (match op
        ['+ +]
        ['- -]
        ['* *]
        ['/ /]
        ['floor floor]
        ['cos cos]
        ['sin sin]
        ['= =]
        ['< <]
        ['> >]
        ['circle interpretCircle]
        ['line interpretLine]
        ['rect interpretRect]
    )
)

(define (zip list1 list2)
    (map (lambda (x y) (cons x y)) list1 list2)
)

; SVG-Tag generators =================================================================================
(define (interpretCircle x y r style) 
    (format "<circle cx=\"~a\" cy=\"~a\" r=\"~a\" style=\"~a\" />" x y r style)
)

(define (interpretLine x1 y1 x2 y2 style) 
    (format "<line x1=\"~a\" y1=\"~a\" x2=\"~a\" y2=\"~a\" style=\"~a\" />" x1 y1 x2 y2 style)
)

(define (interpretRect x y width height style) 
    (format "<rect x=\"~a\" y=\"~a\" width=\"~a\" height=\"~a\" style=\"~a\" />" x y width height style)
)

; Enviroment =========================================================================================
(define empty-env '())

(define (extend-env key value env)
    (cons (cons key value) env)
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

(define (parse-prg prg)
    (map (lambda (def) (cdr def)) prg)
)

(define (lookup key env)
    (cond
        [(null? env) (error (format "No such key in enviroment \n| key: ~a\n" key))]
        [(eq? (caar env) key) (cdar env)]
        [else (lookup key (cdr env))]
    )
)

; Program Evaluation =================================================================================
(define (eval-expr expr env)
    (match expr
        ; arithmetic
        [(list func args ...) #:when (member func num-ops) (execute-func func args env)]
        [(? number?) expr]
        [(? symbol?) (lookup expr env)]
        [(? null?) ""]

        ; argument
        [(? string?) expr]

        ; boolean
        [(list func args ...) #:when (member func bool-ops) (execute-func func args env)]

        ; conditional
        [(list 'if cond then else) (execute-if cond then else env)]
        [(list 'when cond then ..1) (execute-when cond then env)]

        ; application
        [(list func args ...) #:when (member func svg-ops) (execute-func func args env)]
        [(list funcId args ...) #:when (symbol? funcId) (execute-funcId funcId args env)]
        
        ; error
        [else (error (format "Invalid expression \n| expr: ~a\n" expr))]
    )
)

(define (execute-func func args env)
    (define (eval expr) (eval-expr expr env))
    (apply (get-op func) (map eval args))   
)

(define (execute-if cond then else env)
    (if (eval-expr cond env)
        (eval-expr then env)
        (eval-expr else env)
    )
)

(define (execute-when cond thens env)
    (if (eval-expr cond env)
        (flatten (map (lambda (arg) (eval-expr arg env)) thens))
        ""
    )
)

(define (execute-funcId func args env)
    (define func-bodies (cdr (lookup func env))) 
    (define func-args (car (lookup func env)))
    (define eval-vals (map (lambda (arg) (eval-expr arg env)) args))
    (let
        [
            (new-env 
                (foldl 
                    (lambda (pair-n-v result-env) (extend-env (car pair-n-v) (cdr pair-n-v) result-env)) 
                    env 
                    (zip func-args eval-vals)
                )
            )
        ]
        (flatten (map (lambda (body) (eval-expr body new-env)) func-bodies))
    )
)


; Execute ============================================================================================
(define (execute width height prg expr)    
    (let 
        [
            (evaluated-expr (eval-expr expr (env-init prg)))
        ]
        (let
            [
                (svg-string 
                    (if (list? evaluated-expr)
                        (string-join evaluated-expr "")
                        evaluated-expr
                    )
                )
                ;;; (svg-string (string-join (eval-expr expr (env-init prg)) ""))
            ]
            (string-append
                (format "<svg width=\"~a\" height=\"~a\">" width height) 
                svg-string 
                "</svg>"
            )
        )
    )
)

; Testing ============================================================================================
(define test1
    '(
        (define HUNDRED 100)
        (define STYLE "fill:red")
        (define STYLEG "fill:green")
        (define REP 3)
        (define (start a)
            (rect 200 0 a 100 "fill:blue")
            (when (> a 0)
                (start (- a 50))
            ;;;     (rect 0 0 HUNDRED HUNDRED STYLE)
            ;;;     (rect a 0 100 HUNDRED STYLEG)
            )
        )
    )
)

(define test2
  '(
        (define STYLE "fill:red;opacity:0.2;stroke:red;stroke-width:3")
        (define START 195)
        (define END 10)
        (define (circles x r)
            (when (> r END)
                (circle x 200 r STYLE)
                (circles 
                    (+ x (floor (/ r 2))) 
                    (floor (/ r 2))
                )
            )
        )
    )
)

(define tree-prg
'((define STYLE1 "stroke:black;stroke-width:2;opacity:0.9")
    (define STYLE2 "stroke:green;stroke-width:3;opacity:0.9")
    (define FACTOR 0.7)
    (define PI 3.14)
    (define (draw x1 y1 x2 y2 len angle)
    (if (> len 30)
        (line x1 y1 x2 y2 STYLE1)
        (line x1 y1 x2 y2 STYLE2))
    (when (> len 20)
        (recur-tree x2 y2 (floor (* len FACTOR)) angle)
        (recur-tree x2 y2 (floor (* len FACTOR)) (+ angle 0.3))
        (recur-tree x2 y2 (floor (* len FACTOR)) (- angle 0.6))))
    (define (recur-tree x1 y1 len angle)
    (draw x1
            y1
            (+ x1 (* len (cos angle)))
            (+ y1 (* len (sin angle)))
            len
            angle))))
