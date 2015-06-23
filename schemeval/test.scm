(define (f x y) (+ x y))
(f 1 2)

(define (factorial x) (if (= x 1) 1 (* x (factorial (- x 1)))))
(factorial 10)

(define (counter inc) (lambda (x) (set! inc (+ x inc)) inc))
(define my-count (counter 5))
(my-count 3)
(my-count 6)
(my-count 5)
