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

;2-7
(define (make-interval l u)
  (cons l u))
(define (lower-bound f)
  (car f))
(define (upper-bound f)
  (cdr f))
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

;2-10
(define (div-interval x y)
  (if (= (- (upper-bound y) (lower-bound y)) 0)
      0
      (mul-interval x 
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

;2-8
(define (sub-interval x y)
  (add-interval x (make-interval (- 0 (lower-bound y)) (- 0 (upper-bound y)))))

;2-9
;multiply (-1, 1) and (-1, 1) vs multiply (-1, 1) and (5, 7), same with divide

;2-11 cases are, for each interval, lower and upper > 0, < 0, lower <= 0 and upper >= 0 ^2 = 9 choices
(define (mul-interval x y)
  (cond ((and (< (lower-bound x) 0) (< (upper-bound x) 0)))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;2-12
(define (make-center-percent center percentage)
  (make-center-width center (* center (/ percentage 100))))

(define (percent interv)
  (/ (width interv) (center interv)))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))
