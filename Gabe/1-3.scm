(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b) 
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))

(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

;1-29
(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (coefficient k)
    (if (= (remainder k 2) 1)
        4
        (if (or (= k 0) (= k n))
            1
            2)))
  
  (define (next curr) (+ curr h))
  (define (sumwc g coef start nxt end k)
    (if (> start end) 
        0
        (+ (* (g start) (coef k))
           (sumwc g coef (nxt start) nxt end (+ k 1)))))
  (* (sumwc f
            coefficient
            a
            next
            b
            0)
     (/ h 3))) ; simpson gives exact result of 1/4

;1-30
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

;1-31 get test from laptop
(define (product term a next b)
  (if (> a b) 
      1
      (* (term a)
         (sum term (next a) next b))))
(define (pi-form n) ;starts at 1
  (/ (* (+ n 1) (+ n 3))
     (* (+ n 2) (+ n 2))))

;1-32 A.
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

;B.
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

;1-33 A
(define (filtered-accumulate condition combiner null-value term a next b)
  (if (> a b)
      null-value
      (if (condition a)
          (combiner (term a)
                    (accumulate combiner null-value term (next a) next b))
          (combiner null-value
                    (accumulate combiner null-value term (next a) next b)))))

(define (sum-prime-squares a b)
  (filtered-accumulate prime? + 0 square a inc b))

;B
(define (rel-prime a b)
  (= (gcd a b) 1))

(define (sum-rel-prime-n n)
  (filtered-accumulate rel-prime * 1 identity 1 inc (- n 1)))

;1-34
;(f f)
;(f 2)
;(2 2)
;error

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average x y)
  (/ (+ x y) 2))
(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))
;1-35

(define phi 
  (fixed-point (lambda (y) (average y (+ 1 (/ 1 y))))
               0.001))

;1-36
(fixed-point (lambda (y) (/ (log 1000) (log y)))
             2)

(define (cont-frac n d k)
  (define (iter k) 
    (if (= k n)
      (/ (n k) (d k))
      (/ (n k) (+ (d k) (iter (+ k 1))))))
  (iter 1))
