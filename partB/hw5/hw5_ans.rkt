;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body)
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent)

;; Problem 1

;; CHANGE (put your solutions here)

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e)
         (envlookup env (var-string e))]
        [(add? e)
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1)
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; Problem 3

(define (ifaunit e1 e2 e3) "CHANGE")

(define (mlet* lstlst e2) "CHANGE")

(define (ifeq e1 e2 e3 e4) "CHANGE")

;; Problem 4

(define mupl-map "CHANGE")

(define mupl-mapAddN
  (mlet "map" mupl-map
        "CHANGE (notice map is now in MUPL scope)"))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
; 定义一个结构体，e是进行替换后得到的表达式，vars是free variable的集合
; 递归的每一层做两件事，替换表达式和计算freevars的集合
(struct res (e vars))
(define (compute-free-vars e)
  (letrec ([f (lambda (e)
                (cond [(var? e) (res e (set (var-string e)))]
                      [(add? e)
                       (letrec ([res1 (f (add-e1 e))]
                                [res2 (f (add-e2 e))])
                         (res (add (res-e res1) (res-e res2))
                              (set-union (res-vars res1) (res-vars res2))))]
                      [(ifgreater? e)
                       (letrec ([res1 (f (ifgreater-e1 e))]
                                [res2 (f (ifgreater-e2 e))]
                                [res3 (f (ifgreater-e3 e))]
                                [res4 (f (ifgreater-e4 e))])
                         (res (ifgreater (res-e res1) (res-e res2) (res-e res3) (res-e res4))
                              (set-union (res-vars res1) (res-vars res2) (res-vars res3) (res-vars res4))))]
                      [(fun? e)
                       (letrec ([body-res (f (fun-body e))]
                                [bodyenv (set-remove (res-vars body-res)
                                                 (fun-formal e))]
                                [bodyenv (if (fun-nameopt e)
                                             (set-remove bodyenv (fun-nameopt e))
                                             bodyenv)])
                         (res (fun-challenge (fun-nameopt e)
                                             (fun-formal e)
                                             (res-e body-res)
                                             bodyenv)
                              bodyenv))]
                      [(mlet? e)
                       (letrec ([res1 (f (mlet-e e))]
                                [res2 (f (mlet-body e))])
                         (res (mlet (mlet-var e)
                                    (res-e res1)
                                    (res-e res2))
                              (set-union (res-vars res1)
                                         (set-remove (res-vars res2) (mlet-var e)))))]))])
    (res-e (f e))))

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env)
  (cond [(fun-challenge? e)
         (closure (filter env (lambda (p) (set-member? (fun-challenge-freevars e) (car p))))
                  (fun (fun-challenge-nameopt e)
                       (fun-challenge-formal e)
                       (fun-challenge-body e)))]))

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
