#lang racket
(require "lambda-calculus.rkt")

(define expression '((λ x : (y (x x))) (λ y : (y (x x)))))
(define expression2 '(λ y : ((x y) (λ a : (a b)))))

;;; (draw-expr expression2)

(define True `(λ x : (λ y : x)))
(define False `(λ x : (λ y : y)))

(define CONS 
  `(λ a : (λ b : (λ z : ((z a) b))))
)

(define SWAP
    `(λ f : (λ z : ((f (,CONS ((z ,False) ,True))) ,True)))
)

(define pair `((,CONS a) b))

(eval `(,SWAP ((,CONS a) b)))
`(,SWAP ((,CONS a) b))
