#lang racket


"Part 1: Numerical integration"

"Problem 1: Bitdiddle's function"

(define (bitfunc x)
(- (+ (expt x 4) (expt x 2)) 14 ))

"test"
(bitfunc 2)

"Problem 2: A rectangle under Bitdiddle's function"

(define (bitfunc-rect x1 x2)
(abs (* (bitfunc x1) (- x2 x1))))

"test"
(bitfunc-rect 2 3)

"Problem 3: Integrating Bitdiddle's function"

(define (bitfunc-integral-recur num-steps x1 x2)
(if (= num-steps 1)
	(bitfunc-rect x1 x2)
	(+ (bitfunc-rect x1 (+ x1 (/ (- x2 x1) num-steps))) (bitfunc-integral-recur (- num-steps 1) (+ x1 (/ (- x2 x1) num-steps)) x2))
))


(define (bitfunc-integral-iter num-steps x1 x2) 
(bitfunc-integral-iter-helper 0 num-steps x1 x2)
)
(define (bitfunc-integral-iter-helper product num-steps x1 x2)
(if (= num-steps 0)
product
(bitfunc-integral-iter-helper (+ product (bitfunc-rect x1 (+ x1 (/ (- x2 x1) num-steps)))) (- num-steps 1) (+ x1 (/ (- x2 x1) num-steps)) x2)
))

"test"
(bitfunc-integral-recur 100 1 2)
(bitfunc-integral-iter 100 1 2)

"Problem 4: Integrating any function"

(define (integral func num-steps x1 x2) 
(if (= num-steps 1)
	(abs (* (func x1) (- x2 x1)))
	(+ (abs (* (func x1) (- (+ x1 (/ (- x2 x1) num-steps)) x1))) (integral func (- num-steps 1) (+ x1 (/ (- x2 x1) num-steps)) x2))
))

"test"
(integral bitfunc 100 1 2)


"Problem 5: Area of a unit circle"

(define (approx-pi num-steps) 
(* 4 (integral (lambda (x) (sqrt (- 1 (* x x)))) num-steps 0 1))
)

"test"
(approx-pi 100)

"Problem 6: Integrating with pieces of any shape"

(define (rectangle func x1 x2) 
(abs (* (func x1) (- x2 x1)))
)

(define (trapezoid func x1 x2) 
(/ (abs (* (+ (func x1) (func x2)) (- x2 x1))) 2)
)

(define (integral-with piece func num-steps x1 x2) 
(if (= num-steps 1)
	(piece func x1 x2)
	(+ (piece func x1 (+ x1 (/ (- x2 x1) num-steps))) (integral-with piece func (- num-steps 1) (+ x1 (/ (- x2 x1) num-steps)) x2))
))

"test"
(integral-with rectangle bitfunc 100 1 2)
(integral-with trapezoid bitfunc 100 1 2)

"Problem 7: Better approximation of pi"

(define (better-pi num-steps) 
(* 4 (integral-with trapezoid (lambda (x) (sqrt (- 1 (* x x)))) num-steps 0 1))
)

"test"
(better-pi 100) 

"Part 2: Symbolic differentiation"

(define (deriv-constant constant wrt)
    0)


"Problem 8: Derivative of a variable"

(define (deriv-variable var wrt)
(if (eqv? var wrt)
	1
	0
))

"test"
(deriv-variable 'x 'x)
(deriv-variable 'y 'x)


"Problem 9: Calling the right function"

(define (derivative var wrt)
(cond 
((number? wrt ) '0) 
((list? var) (cond ((eqv? (list-ref var 0) '+) (deriv-sum var wrt))
                   ((eqv? (list-ref var 0) '*) (deriv-product var wrt))))
((symbol? wrt) (deriv-variable var wrt))
(else (list "Don't know how to differentiate" 'var))))


"test"
(derivative 3 'x)
(derivative 'x 3)

"Problem 10: Derivative of a sum"

(define (deriv-sum expr wrt)
(list '+ (derivative (list-ref expr 1) wrt) (derivative (list-ref expr 2) wrt)))

"test"
(derivative '(+ x 2) 'x)


"Problem 11: Derivative of a product"

(define (deriv-product expr wrt) 
(list '+ (list '* (list-ref expr 1) (derivative (list-ref expr 2) wrt)) (list '* (list-ref expr 2) (derivative (list-ref expr 1) wrt))))

"test"
(derivative '(* x 3) 'x)

"Problem 12: Additional testing"
(derivative '(+ (* x 1) (* x x)) 'x)
(derivative '(+ (* 1 x) (* y x)) 'x)
(derivative '(+ (* 1 x) (* y x)) 0)
; Additional test cases for 'derivative' go here.
