;1-9
;first is recursive, second is iterative.

;1-10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))
(A 1 10) ;2^10
(A 2 4) ;65536 
(A 3 3) ;65536 

(define (f n) (A 0 n)) ; 2n 

(define (g n) (A 1 n)) ; 2^n

(define (h n) (A 2 n)) ; 2^(h(n - 1))

(define (k n) (* 5 n n)) ;5n^2

;1-11
(define (eff n) 
  (if (< n 3)
      n
      (+ (eff (- n 1))
         (* 2 (eff (- n 2)))
         (* 3 (eff (- n 3))))))

(define (eff n)
  (define (eff-iter m a b c)
    (if (>= m n)
        a
        (eff-iter (+ m 1) (+ a (* 2 b) (* 3 c)) a b)))
  (if (< n 3)
      n
      (eff-iter 2 2 1 0)))

;1-12
(define (pascal n k)
  (if (or (< k 1) (>= k n))
      1
      (+ (pascal (- n 1) (- k 1)) (pascal (- n 1) k))))

;1-13
;trial solution fib(n) = x^n
;characteristic equation: x^2 - x - 1 = 0
;roots f(n) = A((1 + sqrt(5))/2)^n + B((1 - sqrt(5))/2)^n for A and B <- R
;f(0) = A + B = 0
;f(1) = A/phi + B/psi = 1
;solve for A and B, do some algebra, f(n) = (/phi^n -/psi^n)/sqrt(5)

;1-14
;O((#of coins)^n)

;1-15 meh
;

;1-16
(define (square x) (* x x))

(define (fast-expt k l)
  (define (fast-expt-iter b n a)
    (if (= n 0)
        a
        (if (= n 1)
            (if (= a 1)
                b
                a)
            (if (odd? n)
                (fast-expt-iter b
                                (- n 1)
                                (* a b))
                (fast-expt-iter b
                                (/ n 2) 
                                (* a (square b)))))))
  (fast-expt-iter k l 1))

;1-17 and 1-18 (double (mult a (halve b))) = (mult (double a) (halve b))
(define (double x) (+ x x))
(define (halve x) (/ x 2))
(define (fast-mult l m)
  (define (fast-mult-iter a b u)
    (if (= b 0)
        u
        (if (= b 1)
            (if (= u 1)
                a
                u)
            (if (odd? b)
                (fast-mult-iter a
                                (- b 1)
                                (+ u a))
                (fast-mult-iter a
                                (halve b) 
                                (+ u (double a)))))))
  (fast-mult-iter l m 1))

;1-20
;normal order evaluation
;(gcd 206 40)
;(gcd 40 (remainder 206 40))
;(if (= (remainder 206 40) 0)) (if (= 6 0)) 1
;(gcd (remainder 206 40) (remainder 40 (remainder 206 40))) 
;(if (= (remainder 40 (remainder 206 40)) 0)) (if (= 4 0)) 3
;(gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
;(if (= (remainder 40 (remainder 206 40)) 0)) (if (= 2 0)) 7
;15

;applicative order evaluation
;4

;1-21
(define (smallest-divisor n)
  (find-divisor n 2))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))

(smallest-divisor 199) ;199
(smallest-divisor 1999) ;1999
(smallest-divisor 19999) ;7

;1-22 my scheme interpreter does not have a runtime test, so have to find one that does
(define (range start end)
  (if (>= start end)
      '()
      (cons start (range (+ start 1) end))))

;1-23
(define (next n) 
  (if (= n 2)
      3
      (+ n 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

;1-24 no time test again :*(

;1-25
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))
;yeah it seems to be correct?

;1-26 because each one of those calls is recursive, so the branching factor goes up to 2 instead of 1,
;dramatically changing the runtime

;1-27 meh

;1-28 meh later

