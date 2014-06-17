;1.3


(define (sum-of-squares x y) (+ (square x) (square y)))
(define (square x) (* x x))


(define (sum-of-squares x y) 
  (+ (square x) (square y)))

(define (largest-2-square x y z)
  (cond ((> x y z) (sum-of-squares x y))
	((> x z y) (sum-of-squares x z))
	((> y z x) (sum-of-squares y z))
	((> y x z) (sum-of-squares y x))
	((> z x y) (sum-of-squares z x))
	((> z y x) (sum-of-squares z y))))
        
;1.4
;procedure adds the absolute value of b to a
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;1.5
;normal order
;(test 0 (p)) => (if (= 0 0) 0 (p)) => (if #t 0 (p)) => 0 
;applicitive order
;(test 0 (p)) => (test 0 (p)) => (test 0 (p)) => ... => gets caught in an infinite loop evaluating (p)

;1.6 
;infinite loop as the (sqrt-iter (improve guess x) x) is always evalueated before the condition is checked,
;It thus never terminates.

;1.1.7
(define (average x y)
  (/ (+ x y) 2))
(define (improve guess x)
  (average guess (/ x guess)))


(define (good-enough? guess previous x)
  (< (abs (- 1 (/ guess previous))) 0.001))

(define (sqrt-iter guess previous x)
  (if (good-enough? guess previous x)
      guess 
      (sqrt-itr (improve guess x) 
		guess
		x)))

(define (sqrt x)
  (sqrt-iter 1.0 100.0 x))

(define (improve-cube guess x)
  (average guess (/ x guess)))

(define (improve guess x)
  (/ (+ guess (/ x guess)))

(define (cube-iter guess previous x)
  (if (good-enough? guess previous x)
      guess 
      (sqrt-itr (improve-cube guess x)
		guess
		x)))

(define (cube x)
  (sqrt-iter 1.0 100.0 x))
