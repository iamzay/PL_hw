
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (<= low high)
      (cons low (sequence (+ low stride) high stride))
      null))

(define (string-append-map xs suffix)
  (map (lambda (str) (string-append str suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (letrec ([i (remainder n (length xs))])
              (car (list-tail xs i)))]))

(define (stream-for-n-steps stream n)
  (if (= n 0)
      null
      (letrec ([cur (stream)])
        (cons (car cur) (stream-for-n-steps (cdr cur) (- n 1))))))

(define funny-number-stream
  (letrec ([f (lambda (x)
                (letrec ([num (if (= (remainder x 5) 0)
                                  (- 0 x)
                                  x)])
                  (cons num (lambda () (f (+ 1 x))))))])
    (lambda () (f 1))))

(define dan-then-dog
  (letrec ([f (lambda (s)
                (letrec ([next (if (equal? s "dan.jpg") "dog.jpg" "dan.jpg")])
                  (cons s (lambda () (f next)))))])
    (lambda () (f "dan.jpg"))))

(define (stream-add-zero s)
  (letrec ([f (lambda (stream)
                (letrec ([cur (stream)])
                  (cons (cons 0 (car cur)) (lambda () (f (cdr cur))))))])
    (lambda () (f s))))

(define (cycle-lists xs ys)
  (letrec ([len1 (length xs)]
           [len2 (length ys)]
           [f (lambda (n1 n2)
                (cons (cons (list-nth-mod xs n1) (list-nth-mod ys n2))
                      (lambda () (f (remainder (+ 1 n1) len1) (remainder (+ 1 n2) len2)))))])
    (lambda () (f 0 0))))

(define vector-assoc
  (letrec ([f (lambda (i v vec n)
                (if (= i n)
                    #f
                    (letrec ([cur (vector-ref vec i)])
                      (cond [(and (pair? cur) (equal? (car cur) v)) cur]
                            [#t (f (+ 1 i) v vec n)]))))])
    (lambda (v vec) (f 0 v vec (vector-length vec)))))

(define (cached-assoc xs n)
  (letrec ([cache (make-vector n)]
           [pos 0])
    (lambda (v) (letrec ([res (vector-assoc v cache)])
                  (if res res (letrec ([new-res (assoc v xs)])
                                (if (not new-res)
                                    #f
                                    (begin (vector-set! cache pos new-res)
                                           (set! pos (remainder (+ 1 pos) n))
                                           new-res))))))))

(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([end e1]
              [begin e2])
       (letrec ([f (lambda (x)
                     (if (>= x end)
                         #t
                         (f e2)))])
         (f begin)))]))

