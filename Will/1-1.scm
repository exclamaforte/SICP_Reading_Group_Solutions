; Exercise 1.1
; 10 -> 10
; (+ 5 3 4) -> 12
; (- 9 1) -> 8
; (/ 6 2) -> 3
; (+ (* 2 4) (- 4 6)) -> (+ 8 -2) -> 6
; (define a 3) [a = 3]
; (define b (+ a 1)) [b = 4]
; (+ a b (* a b)) -> (+ 3 4 12) -> 19
; (= a b) -> false
; (if (and (> b a) (< b (* a b)))
;   b
;   a) -> (if (true && true) b a) -> b
; (cond ((= a 4) 6)
;      ((= b 4) (+ 6 7 a))
;      (else 25)) -> 17 
; (+ 2 (if (> b a) b a)) -> (+ 2 4) -> 6
; (* (cond ((> a b) a)
;         ((< a b) b)
;         (else -1))
;   (+ a 1)) -> (* 4 4) -> 16

; Exercise 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

; Exercise 1.3
(define (sum_squares x y)
    (+ (* x x) (* y y))
)

(define (three_number_magic x y z)
    (cond 
        ((> x y z) (sum_squares x y))
        ((> x z y) (sum_squares x z))
        ((> y x z) (sum_squares y x))
        ((> y z x) (sum_squares y z))
        ((> z x y) (sum_squares z x))
        ((> z y x) (sum_squares z y))
    )
)

; Exercise 1.4
; Easier to express purpose with a translation to generic C-style function
; double add_ignore_sign(double a, double b)
; { return (b > 0) ? a + b : a - b ; }

; Exercise 1.5
; Applicative order: output = 0
; Normal order: WTF BOOM since 'p' ain't really defined

; Exercise 1.6
; ANSWER: At some point, there will come a time where both parts of the conditional fail.
; Then, the value of the conditional is undefined!

; functions:
; tolerance limits for sqrt approx
(define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001)
)

; improve guess func
(define (improve guess x)
    (average guess (/ x guess)))
  
(define (average x y) 
    (/ (+ x y) 2)) 
    
; iterative [recursive] approach for sqrts
(define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)
    )
)
      
; Exercise 1.7
; Execute one step look ahead. Good enough is when the improved guess is
; small percentage of old guess
(define (new_good_enough guess x)
    (< (abs (- (improve guess) (guess))) (* 0.001 x))
)
; this should be more reliable for small and large numbers 
;at the possible cost of accuracy for "in the middle" numbers.

; Exercise 1.8
(define EPSILON 0.0001)

(define (cube x) (* x x x))

(define (in_tolerance guess x)
    (< (abs (- (cube guess) x)) EPSILON))

(define (cube_root_improve guess x)
    (/ (+ (/ (x) (guess * guess)) (* 2 guess)) 3))
    
(define (cube_root_approx guess x)
    (if (in_tolerance guess x)
        guess
        (cube_root_approx (cube_root_improve guess) x)
    )
)
