;2-1
(define (make-rat n d) 
  (let ((g (gcd n d)))
    (if (< d 0)
        (cons (- 0 (/ n g)) (- 0 (/ d g)))
        (cons (/ n g) (/ d g)))))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;2-2
(define (make-segment x1 x2) 
  (cons x1 x2))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))
(define (make-point x y)
  (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (average x y)
  (/ (+ x y) 2))
(define (midpoint-segment line)
  (make-point (average (x-point (start-segment line)) 
                       (x-point (end-segment line)))
              (average (y-point (start-segment line)) 
                       (y-point (end-segment line)))))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;2-3
(define (make-rect x1 x2)
  (make-segment x1 x2))
(define (area rect)
  (* (- (x-point (start-segment line)) 
        (x-point (end-segment line)))
     (- (y-point (start-segment line)) 
        (y-point (end-segment line)))))

(define (perimeter rect)
  (* (+ (- (x-point (start-segment line)) 
           (x-point (end-segment line)))
        (- (y-point (start-segment line)) 
           (y-point (end-segment line))))
     2))

;2-4
;(define (cons x y)
;  (lambda (m) (m x y)))

;(define (car z)
;  (z (lambda (p q) p)))

;(define (cdr z)
;  (z (lambda (p q) q)))

;2-5
(define (power n k)
  (if (= k 0)
      1
      (* n (power n (- k 1)))))
(define (cns a b)
  (* (power 2 a) (power 3 b)))

(define (ca n)
  (define (iter m curr)
    (if (odd? m)
        curr
        (iter (/ m 2) (+ curr 1))))
  (iter n 0))

(define (cd n)
  (define (iter m curr)
    (if (not (= (remainder m 3) 0))
        curr
        (iter (/ m 3) (+ curr 1))))
  (iter n 0))

;2-6 seems hard
