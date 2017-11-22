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
(define (racketlist->mupllist xs)
  (if (null? xs)
      (aunit)
      (apair (car xs) (racketlist->mupllist (cdr xs)))))

;; Problem 2
(define (mupllist->racketlist xs)
  (if (aunit? xs)
      null
      (cons (apair-e1 xs) (mupllist->racketlist (apair-e2 xs)))))

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
        [(int? e) e]
        [(closure? e) e]
        [(fun? e) (closure env e)]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]
        [(mlet? e)
         (let ([s (mlet-var e)]
               [v (eval-under-env (mlet-e e) env)])
           (eval-under-env (mlet-body e)
                           (append env (list (cons s v)))))]
        [(call? e)
         (letrec ([v1 (eval-under-env (call-funexp e) env)]
                  [v2 (eval-under-env (call-actual e) env)])
           (if (closure? v1)
               (letrec ([func (closure-fun v1)]
                     [func-name (fun-nameopt func)]
                     [arg-name (fun-formal func)]
                     [env (closure-env v1)]
                     [extend-env (append
                                  env
                                  (list (cons arg-name v2))
                                  (if func-name
                                      (list (cons func-name v1))
                                      (list)))])
                 (eval-under-env (fun-body func) extend-env))
               (error "MUPL call applied to non-function")))]
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "MUPL fst applied to non-apair")))]
        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error "MUPL snd applied to non-apair")))]
        [(isaunit? e)
         (let ([v (eval-under-env (isaunit-e e) env)])
           (if (aunit? v)
               (int 1)
               (int 0)))]
        [(aunit? e) e]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; Problem 3

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (mlet (car (car lstlst))
            (cdr (car lstlst))
            (mlet* (cdr lstlst) e2))))

(define (ifeq e1 e2 e3 e4)
  (mlet "_x" e1
        (mlet "_y" e2
              (ifgreater (var "_x") (var "_y")
                         e4
                         (ifgreater (var "_y") (var "_x")
                                    e4
                                    e3)))))

;; Problem 4

(define mupl-map
  (fun "new-map" "map-func"
       (fun "new-map2" "map-list"
            (ifaunit (var "map-list")
                     (aunit)
                     (apair (call (var "map-func") (fst (var "map-list")))
                            (call (var "new-map2") (snd (var "map-list"))))))))

(define mupl-mapAddN
  (mlet "map" mupl-map
        (fun "f1" "integer"
             (fun "f2" "mupl-list"
                  (call (call (var "map")
                              (fun "map-func" "elem"
                                   (add (var "integer") (var "elem"))))
                        (var "mupl-list"))))))
; 上面有不必要的函数wrapper
(define mupl-mapAddN2
  (mlet "map" mupl-map
        (fun "f" "integer"
             (call (var "map")
                   (fun "f1" "elem"
                        (add (var "integer") (var "elem")))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e)
  (letrec ([collect-vars (lambda (e)
                           (letrec ([free-vars
                                     (cond [(var? e) (set (var-string e))]
                                           [(add? e)
                                            (set-union (collect-vars (add-e1 e))
                                                       (collect-vars (add-e2 e)))]
                                           [(ifgreater? e)
                                            (set-union (collect-vars (ifgreater-e1 e))
                                                       (collect-vars (ifgreater-e2 e))
                                                       (collect-vars (ifgreater-e3 e))
                                                       (collect-vars (ifgreater-e4 e)))]
                                           [(fun? e)
                                            (set-subtract (collect-vars (fun-body e))
                                                          (set (fun-formal e)
                                                               (or (fun-nameopt e) "??")))]
                                           [(call? e)
                                            (set-union (collect-vars (call-funexp e))
                                                       (collect-vars (call-actual e)))]
                                           [(mlet? e)
                                            (set-union (collect-vars (mlet-e e))
                                                       (set-remove (collect-vars (mlet-body e)) (mlet-var e)))]
                                           [(apair? e)
                                            (set-union (collect-vars (apair-e1 e))
                                                       (collect-vars (apair-e2 e)))]
                                           [(fst? e) (collect-vars (fst-e e))]
                                           [(snd? e) (collect-vars (snd-e e))]
                                           [(isaunit? e) (collect-vars (isaunit-e e))]
                                           [#t (set)])])
                             free-vars))])
    (cond [(add? e) (add (compute-free-vars (add-e1 e))
                         (compute-free-vars (add-e2 e)))]
          [(fun? e) (fun-challenge (fun-nameopt e)
                                   (fun-formal e)
                                   (compute-free-vars (fun-body e))
                                   (collect-vars e))]
          [(ifgreater? e) (ifgreater (compute-free-vars (ifgreater-e1 e))
                                     (compute-free-vars (ifgreater-e2 e))
                                     (compute-free-vars (ifgreater-e3 e))
                                     (compute-free-vars (ifgreater-e4 e)))]
          [(call? e) (call (compute-free-vars (call-funexp e))
                           (compute-free-vars (call-actual e)))]
          [(mlet? e) (mlet (mlet-var e)
                           (compute-free-vars (mlet-e e))
                           (compute-free-vars (mlet-body e)))]
          [(apair? e) (apair (compute-free-vars (apair-e1 e))
                             (compute-free-vars (apair-e2 e)))]
          [(fst? e) (fst (compute-free-vars (fst-e e)))]
          [(snd? e) (snd (compute-free-vars (snd-e e)))]
          [(isaunit? e) (isaunit (compute-free-vars (isaunit-e e)))]
          [#t e])))

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env)
  (cond [(var? e)
         (envlookup env (var-string e))]
        [(add? e)
         (let ([v1 (eval-under-env-c (add-e1 e) env)]
               [v2 (eval-under-env-c (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1)
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        [(int? e) e]
        [(closure? e) e]
        [(fun-challenge? e)
         (letrec ([s (fun-challenge-freevars e)]
                  [get-env (lambda ()
                             (filter (lambda (p)
                                       (set-member? s (car p)))
                                     env))]
                  [func-name (fun-challenge-nameopt e)]
                  [formal-name (fun-challenge-formal e)]
                  [func-body (fun-challenge-body e)]
                  [new-fun (fun func-name formal-name func-body)])
           (closure (get-env) new-fun))]
        [(ifgreater? e)
         (let ([v1 (eval-under-env-c (ifgreater-e1 e) env)]
               [v2 (eval-under-env-c (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env-c (ifgreater-e3 e) env)
                   (eval-under-env-c (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]
        [(mlet? e)
         (let ([s (mlet-var e)]
               [v (eval-under-env-c (mlet-e e) env)])
           (eval-under-env-c (mlet-body e)
                           (append env (list (cons s v)))))]

        [(call? e)
         (letrec ([v1 (eval-under-env-c (call-funexp e) env)]
                  [v2 (eval-under-env-c (call-actual e) env)])
           (if (closure? v1)
               (letrec ([func (closure-fun v1)]
                     [func-name (fun-nameopt func)]
                     [arg-name (fun-formal func)]
                     [env (closure-env v1)]
                     [extend-env (append
                                  env
                                  (list (cons arg-name v2))
                                  (if func-name
                                      (list (cons func-name v1))
                                      (list)))])
                 (eval-under-env-c (fun-body func) extend-env))
               (error "MUPL call applied to non-function")))]
        [(apair? e)
         (let ([v1 (eval-under-env-c (apair-e1 e) env)]
               [v2 (eval-under-env-c (apair-e2 e) env)])
           (apair v1 v2))]
        [(fst? e)
         (let ([v (eval-under-env-c (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "MUPL fst applied to non-apair")))]
        [(snd? e)
         (let ([v (eval-under-env-c (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error "MUPL snd applied to non-apair")))]
        [(isaunit? e)
         (let ([v (eval-under-env-c (isaunit-e e) env)])
           (if (aunit? v)
               (int 1)
               (int 0)))]
        [(aunit? e) e]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
