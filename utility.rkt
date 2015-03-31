;#lang racket
(define (ask object message . args)
  (let ((method (get-method object message)))
    (if (method? method)
        (apply method (cons object args))
        (error "No method" message (cadr method)))))

(define (get-method object message)
  (object message))

(define (no-method name)
  (list 'no-method name))

(define (method? x)
  (not (no-method? x)))

(define (no-method? x)
  (if (pair? x)
      (eq? (car x) 'no-method)
      #f))

;;; --------------------------------------------------------------------------
;;; Miscellaneous procedures

(define (is-a object property)
  (let ((method (get-method object property)))
    (if (method? method)
        (ask object property)
        false)))


(define (display-message list-of-stuff)
  (newline)
  (for-each (lambda (s) (display s) (display " "))
            list-of-stuff))

(define (filter predicate lst)
  (cond ((null? lst) '())
        ((predicate (car lst))
         (cons (car lst) (filter predicate (cdr lst))))
        (else (filter predicate (cdr lst)))))


(define (pick-random lst)
  (if (null? lst)
      false
      (list-ref lst (random (length lst)))))  ;; See manual for LIST-REF

(define (delq item lst)
  (cond ((null? lst) '())
        ((eq? item (car lst)) (delq item (cdr lst)))
        (else (cons (car lst) (delq item (cdr lst))))))

(define (any-in-play? player)
  (foldl (lambda (x y)(or x y)) #f (map in-play? (ask player 'get-tokens))))

(define (in-play? token)
  (cons (ask token 'name)
        (not(= 0 (remainder (ask token 'position) 100)))))