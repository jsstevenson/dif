;;; Dif tests
;;; James Stevenson jssteven@uw.edu
#lang racket

(require rackunit)
(require rackunit/text-ui)
(require "dif.rkt")

(define basic-cases
  (test-suite "Basic cases"
              (check-equal? (diff 'x 5) 0
                            "Single constant")
              (check-equal? (diff 'x 'y) 0
                            "Single constant (non-differentiated variable)")
              (check-equal? (diff 'x 'x) 1
                            "Single variable")))

(define sums
  (test-suite "Differentiating sums"
              (check-equal? (diff 'x '(+ 1 5)) 0
                            "Two constants")
              (check-equal? (diff 'x '(+ 1 (+ 2 3))) 0
                            "A nested constant term")
              (check-equal? (diff 'x '(+ (+ 5 6) (+ 7 8))) 0
                            "Two nested constant terms")
              (check-equal? (diff 'x '(+ x 5)) '(+ 1 0)
                            "One variable term")
              (check-equal? (diff 'x '(+ (+ x y) 6)) '(+ (+ 1 0) 0)
                            "Nested variable term w/ non-differentiated var")
              (check-equal? (diff 'x '(+ 5 (+ 6 (+ 7 x)))) '(+ 0 (+ 0 (+ 0 1)))
                            "Multi-level nesting, variable term")))

(define products
  (test-suite "Differentiating products"
              (check-equal? (diff 'x '(* 6 6)) 0
                            "Two constants")
              (check-equal? (diff 'x '(* 6 x)) '(+ (* 0 x) (* 6 1))
                            "One variable")
              (check-equal? (diff 'x '(* x x)) '(+ (* 1 x) (* x 1))
                            "Product of variables")
              (check-equal? (diff 'x '(* (+ 6 7) x)) '(+ (* 0 x) (* (+ 6 7) 1))
                            "Nested sum product w/ variable")
              (check-equal? (diff 'x '(+ (* 5 x) (* 4 x))) '(+ (+ (* 0 x) (* 5 1)) (+ (* 0 x) (* 4 1)))
                            "Sum of two nested variable terms")
              (check-equal? (diff 'x '(* 5 (* 6 x))) '(+ (* 0 (* 6 x)) (* 5 (+ (* 0 x) (* 6 1))))
                            "Nested product of variable & constant")
              (check-equal? (diff 'x '(* (* x 6) 5)) '(+ (* (+ (* 1 6) (* x 0)) 5) (* (* x 6) 0))
                            "Nested product of variable and constant, different positions")))

(define exponents
  (test-suite "Differentiating exponents"
              (check-equal? (diff 'x '(expt 5 1)) 0
                            "Two constants")
              (check-equal? (diff 'x '(expt x 4)) '(* 4 (expt x 3))
                            "Variable base")
              (check-equal? (diff 'x '(expt x 0)) '(* 0 (expt x -1))
                            "0 power")
              (check-equal? (diff 'x '(expt x -2)) '(* -2 (expt x -3))
                            "Negative power")
              (check-equal? (diff 'x '(expt (+ 5 x) 2)) '(* 2 (* (expt (+ 5 x) 1) (+ 0 1)))
                            "Chain rule/variable expression in ")))

(define trig
  (test-suite "Differentiating trig functions"

              (check-equal?
                (diff 'x '(sin 5))
                0
                "Sin constant")

              (check-equal?
                (diff 'x '(sin x))
                '(* (cos x) 1)
                "Sin simple variable")

              (check-equal?
                (diff 'x '(sin (* 5 x)))
                '(* (cos (* 5 x)) (+ (* 0 x) (* 5 1)))
                "Sin chain rule")

              (check-equal?
                (diff 'x '(cos 5))
                0
                "Cos constant")

              (check-equal?
                (diff 'x '(cos x))
                '(* (sin x) (* -1 1))
                "Cos simple variable")

              (check-equal?
                (diff 'x '(cos (+ x 5)))
                '(* (sin (+ x 5)) (* -1 (+ 1 0)))
                "Cos chain rule")

              (check-equal?
                (diff 'x '(tan 5))
                0
                "Tan constant")

              (check-equal?
                (diff 'x '(tan x))
                '(* (expt (sec x) 2) 1)
                "Tan simple variable")

              (check-equal?
                (diff 'x '(tan (expt x 4)))
                '(* (expt (sec (expt x 4)) 2) (* 4 (expt x 3)))
                "Tan chain rule")

              (check-equal?
                (diff 'x '(csc 5))
                0
                "Cosecant constant")

              (check-equal?
                (diff 'x '(csc x))
                '(* (* -1 (* (csc x) (cot x))) 1)
                "Cosecant simple variable")

              (check-equal?
                (diff 'x '(csc (+ (* 3 x) 2)))
                '(* (* -1 (* (csc (+ (* 3 x) 2)) (cot (+ (* 3 x) 2)))) (+ (+ (* 0 x) (* 3 1)) 0))
                "Cosecant chain rule")

              (check-equal?
                (diff 'x '(sec y))
                0
                "Secant constant")

              (check-equal?
                (diff 'x '(sec x))
                '(* (* (sec x) (tan x)) 1)
                "Secant simple variable")

              (check-equal?
                (diff 'x '(sec (expt x 4)))
                '(* (* (sec (expt x 4)) (tan (expt x 4))) (* 4 (expt x 3)))
                "Secant chain rule")

              (check-equal?
                (diff 'x '(cot 4))
                0
                "Cotangent constant")

              (check-equal?
                (diff 'x '(cot x))
                '(* (* (expt (csc x) 2) -1) 1)
                "Cotangent simple variable")

              (check-equal?
                (diff 'x '(cot (+ 5 x)))
                '(* (* (expt (csc (+ 5 x)) 2) -1) (+ 0 1))
                "Cotangent chain rule")
              ))

(define simpl-cases
  (test-suite "Simplify"
              (check-equal?
                (simpl 'x (list + (+ 1 1) 1))
                3)
              (check-equal?
                (diff 'x '(* x (+ 1 2)))
                3)
                ))


(run-tests basic-cases)
(run-tests sums)
(run-tests products)
(run-tests exponents)
(run-tests trig)
(run-tests simpl-cases)
