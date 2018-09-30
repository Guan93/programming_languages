
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

; Problem 1
(define (sequence low high stride)
  (if (> low high)
    null
    (cons low (sequence (+ low stride) high stride))))

; Problem 2
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

; Problem 3
(define (list-nth-mod xs n)
  (cond [(negative? n) (error "list-nth-mod: negative number")]
        [(= 0 (length xs)) (error "list-nth-mod: empty list")]
        [#t (let ([i (remainder n (length xs))])
              (car (list-tail xs i)))]))

; Problem 4
(define (stream-for-n-steps s n)
  (if (= n 0)
    null
    (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

; Problem 5
(define funny-number-stream
  (letrec ([divisble5 (lambda (x) 
                        (if (= 0 (remainder x 5))
                               (- x)
                               x))]
           [f (lambda (x) (cons (divisble5 x) (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

; Problem 6
(define dan-then-dog
  (letrec ([dan (lambda () (cons "dan.jpg" dog))]
           [dog (lambda () (cons "dog.jpg" dan))])
    (lambda () (dan))))

; Problem 7
(define (stream-add-zero s)
  (letrec ([v (car (s))]
           [s1 (cdr (s))])
    (lambda () (cons (cons 0 v) (stream-add-zero s1)))))

; Problem 8
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (letrec ([x (list-nth-mod xs n)]
                         [y (list-nth-mod ys n)])
                  (cons (cons x y) (lambda () (f (+ n 1))))))])
    (lambda () (f 0))))

; Problem 9
(define (vector-assoc v vec)
  (letrec ([vl (vector-length vec)]
           [f (lambda (n)
                (if (= vl n) 
                  #f 
                  (letrec ([p (vector-ref vec n)]) 
                    (if (and (pair? p) (equal? v (car p))) 
                      p 
                      (f (+ n 1))))))])
    (f 0)))

; Problem 10
(define (cached-assoc xs n)
  (letrec ([i 0]
           [cache (make-vector n #f)])
    (lambda (v)
      (letrec ([p_fast (vector-assoc v cache)])
        (if p_fast
          p_fast
          (letrec ([p_slow (assoc v xs)])
            (if p_slow
              (begin 
                (vector-set! cache i p_slow)
                (if (= i (- n 1))
                  (set! i 0)
                  (set! i (+ i 1)))
                p_slow)
              #f)))))))
