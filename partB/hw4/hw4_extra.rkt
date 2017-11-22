#lang racket
(require "hw4.rkt")
(require "definition.rkt")
(define (palindromic xs)
  (map (lambda (n1 n2)
         (+ n1 n2))
       xs
       (reverse xs)))

(define fibonacci
  (letrec ([f (lambda (a b)
                (cons (+ a b) (lambda () (f b (+ a b)))))])
    (lambda () (f -1 1))))

(define (stream-until f s)
  (letrec ([cur (s)])
    (if (f (car cur))
        #t
        (stream-until f (cdr cur)))))

(define (stream-map f s)
  (letrec ([helper (lambda (s)
                (letrec ([cur (s)])
                  (cons (f (car cur))
                        (lambda () (helper (cdr cur))))))])
    (lambda () (helper s))))

(define (stream-zip s1 s2)
  (letrec ([f (lambda (s1 s2) (letrec ([val1 (s1)]
                                       [val2 (s2)])
                                (cons (cons (car val1) (car val2))
                                      (lambda () (f (cdr val1) (cdr val2))))))
              ])
    (lambda () (f s1 s2))))

; method 1: 将当前处理完的stream添加到list末尾
(define (interleave xs)
  (letrec ([f (lambda (xs pos)
                (letrec ([s (list-ref xs pos)]
                         [cur (s)])
                  (cons (car cur)
                        (lambda ()
                          (f (append xs (list (cdr cur))) (+ 1 pos))))))])
    (lambda () (f xs 0))))

; method 2: 记录两个位置，一个指示当前的stream，一个指示要获取stream中哪个位置的值
(define (interleave2 xs)
  (letrec ([f (lambda (i j)
                (letrec ([s (list-ref xs i)]
                         [cur-val (list-ref (stream-for-n-steps s (+ 1 j)) j)])
                  (cons cur-val (lambda ()
                                  (f (remainder (+ 1 i) len)
                                     (if (= i (- len 1)) (+ 1 j) j))))))]
           [len (length xs)])
    (lambda () (f 0 0))))

(define (pack n s)
  (letrec ([f (lambda (xs s i)
                (if (= i n)
                    (cons xs s)
                    (letrec ([cur (s)])
                      (f (append xs (list (car cur)))
                         (cdr cur)
                         (+ i 1)))))]
           [f1 (lambda (s)
                 (letrec ([p (f (list) s 0)])
                   (cons (car p)
                         (lambda () (f1 (cdr p))))))])
    (lambda () (f1 s))))

(define (sqrt-stream n)
  (letrec ([f (lambda (x)
                (cons x (lambda ()
                          (f (* 0.5 (+ x (/ n x)))))))])
    (lambda () (f n))))

(define (approx-sqrt n e)
  (letrec ([s (sqrt-stream n)]
           [f (lambda (cur-stream)
                (letrec ([cur (cur-stream)]
                         [val (car cur)]
                         [next (cdr cur)])
                  (if (< (abs (- (* val val) n)) e)
                      val
                      (f next))))])
    (f s)))

(define-syntax perform
  (syntax-rules (if unless)
    [(perform e1 if e2)
     (letrec ([val2 e2])
       (if val2 e1 val2))]
    [(perform e1 unless e2)
     (letrec ([val2 e2])
       (if val2 val2 e1))]))

