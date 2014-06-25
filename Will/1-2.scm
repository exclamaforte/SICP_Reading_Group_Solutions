; Exercise 1.9
(define (inc a) (+ a 1))
(define (dec a) (- a 1))

(define (+ a b)
    (if (= a 0)
        b
        (inc (+ (dec a) b))
    )
)
; (+ 4 5) -> (inc (+ 3 5)) -> (inc (inc (+ 2 5)))
; -> (inc (inc (inc (+ 1 5)))) -> (inc (inc (inc (inc 5)))) -> 9
; This is a recursive method

(define (+ a b)
    (if (= a 0)
        b
        (+ (dec a) (inc b))
    )
)

; (+ 4 5) -> (+ 3 6) -> (+ 2 7) -> (+ 1 8) -> (+ 0 9) -> 9
; this is iterative

; Exercise 1.10
; (A 1 10) -> (A (0 (A 1 9))) -> (A (0 (A 0 (A 0 8)))) -> ... ->
; (A 0 (A 0 ( ... (A 0 1) ... )) -> 2 * 2 * ... * 2 = 2^10 = 1024

; 2 3 -> 1 [2 2] -> [1 [1 [2 1]]] -> [1 [1 [1 [2 0]]]] ->
; (A 2 4) -> (A 1 (A (1 ... (A 1 (A (2 0))) ... ))) ->
; (A 1 (A (1 ... (A 1 0) ... ))) -> 4 (A 1 0) exp -> 2^(2^4) = 2^16

; (A 3 3)

; (define (f n) (A 0 n)) -> 2 * n
; (define (g n) (A 1 n)) -> 2^n
; (define (h n) (A 2 n)) -> 2^(A 2 (- n 1))

; Exercise 1.11
(define (count-rec n)
    (if (< n 3)
        n
        (+ 
           (count-rec (- n 1)) 
           (* 2 (count-rec (- n 2))) 
           (* 3 (count-rec (- n 3)))
        )
    )
)

(define (count-itr n)
    () ; this is going to be a pain
)

; Exercise 1.12
(define (pascal n)
    (cond 
        (<= n 2) (1)
        ; not quite
        (else (+ (pascal (floor (/ n 2))) (pascal (+ floor(/ n 2) 1))))
    )
)

; Exercise 1.13
; Base case: 
; Fib(1) = 1
; By formula: phi^1 / sqrt(5) is about 1
;
; Inductive case: (strong)
; if Fib(n) = k = phi^n / sqrt(5) holds for all integers > 0, prove Fib(n + 1) = phi^(n + 1) / sqrt(5)
; Fib(n) = Fib(n - 2) + Fib(n - 1)
; Fib(n + 1) = Fib(n) + Fib(n - 1)
; By inductive hypothesis, Fib(n + 1) is phi^(n + 1) / sqrt(5). [end horrible induction]

; Exercise 1.14
;                   11
;                    |
;                 (11, 5)
;                   
; .... this is going to get big because friggin pennies

; Exercise 1.15
; 1. 12.15 -> 4.05 -> 1.35 -> 0.45 -> 0.15 -> 0.05 [5 times]
; 2. space: O(a) steps: O(a)

; Exercise 1.16
(define (fast-expt b n)
    (cond 
        ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))
    )
)

; Raise a^b iteratively
(define (square n) (* n n))

(define (fastexpt_itr b n)
    (fastexpt_itr b n 1)
)

(define (fastexpt_helper b n a)
    (cond
        (= n 1) (* a b)
        (even? n) (fastexpt_helper (b) (/ n 2) (* a (square b)))
        (odd? n) (fastexpt_helper (b) (- n 1) (* a b))
    )
)

; Exercise 1.17 and 1.18
; does not handle negative numbers at all
(define (* a b) 
    (define (mult-itr a b val)
        (define (double x) (+ x x))
        (define (halve x) (/ x 2))
        (cond
            ; if there are zeros, return 0
            [(or (= a 0) (= b 0)) 0]
            
            ; if a is 1 but b is not, then run method with b,a reversed.
            [(and (= a 1) (not (= b 1))) (mult-itr b a val)]
            
            [(if (odd? b)
                (if (= b 1) 
                    a
                    (mult-itr a (- b 1) (+ val a))
                )
                
                (+ val (double (mult-itr a (halve b) 0)))
            )]
        )
    )
    (mult-itr a b 0)
)


