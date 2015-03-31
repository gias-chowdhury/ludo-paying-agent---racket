#lang racket
(require db)
(require racket/include)
(include "world.rkt")
(define myconn ;;grab the result of a connection
  (mysql-connect #:user "me"
                 #:database "test"))

;;;;;;setup:
(define tablename "trial")
(define basic-format "turn int, player varchar(10), 1 int, 2 int, 3 int, 4 int")
(define basic-order "turn , player , 1 , 2 , 3 , 4 ")
(define (new-table format name)
  (set! tablename name)
  (if (null? format)
      (query-exec myconn (string-append "CREATE TABLE "
                                 name "( " basic-format ")"))
      (query-exec myconn (string-append "CREATE TABLE "
                                 name "( " (make-into-string format) ")"))
      ))

;;;;;;tools to work with the engine:
;(define (bulk-export board turn)
;  (for-each  (lambda (n) (export-player n turn)) (ask board 'players)))
;;warning bulk-export and bulk-store do not work as for-each does not give an unlinked sequence of items
;(define (bulk-store board turn)
;  (bulk-insert tablename (bulk-export board turn)))
;;a possible solution would be to (apply (values (take head) . (drop tail))

(define (do-bulk board turn)
  (for-each (lambda (arg)
              (insert-turn tablename
                           (export-player arg turn)))
              (ask board 'players)))


(define (export-player player turn)
    (cons (cons 'turn turn) (cons (cons 'player (ask player 'name))
    (map (lambda (n)
           (cons (ask n 'name)
                 (ask n 'position))
           ) (ask player 'get-tokens)))))




(define which-player-from-data (lambda (board data)
    (car(filter (lambda(n)(equal? (ask n 'name) (get-by-tag 'player data))) (ask board 'players)))))

(define (send-to-engine lst board)
  (define (token-list lst board)
    (ask (which-player-from-data board lst) 'get-tokens))
  
  (define (find-new-data dest-tag data)
    (get-by-tag dest-tag data))
  
  (for-each ( lambda (arg)
                            (ask arg 'set-position 
                                 (find-new-data (ask arg 'name) lst)))
            (token-list lst board)))


;;;;;;type and tag coersions:

(define (default-format n)
          (lambda (n)
            (string->symbol n)))

(define (make-into-escaped-string thing)
  (cond ((symbol? thing) (string-append "'" (symbol->string thing) "'"))
        ((number? thing) (number->string thing))
        ((string? thing) (string-append "'" thing "'"))
        (else default-format)))

(define (make-into-string thing)
  (cond ((symbol? thing) (symbol->string thing)
        ((number? thing) (number->string thing))
        ((string? thing) thing))
        (else (default-format thing))))


(define (type-cheat n)
  (cond ((number? n) (cond ((>= n 0) n) 
                           ((= n -1) 'Start)
                           ((= n -2) 'Home)
                           ))))
        ;((symbol? n) (string-append "'" (symbol->string n) "'"))))
                           

(define tag-type (list (cons 'turn (lambda (n) n))
                       (cons 'player make-into-escaped-string)
                       (cons 'pawn1 (lambda (n) type-cheat n))
                       (cons 'pawn2 type-cheat)
                       (cons 'pawn3 type-cheat)
                       (cons 'pawn4 type-cheat)
))

(define easy (list 'turn 'player 'pawn1 'pawn2 'pawn3 'pawn4))

(define (make-attrib-order lst)
  (string-append* (cdr (append* (map (lambda (x) (list ", " x))
                                     (map (lambda (n) (make-into-string (car n)))lst))))))

  
  
;(define order (make-attrib-order tag-type))
  
(define (get-by-tag tag stuff)
  ;for getting data by searching for a tag  
    (cdr (car (filter 
          (lambda (pair)
            (eq? tag (car pair)));is this the tag you are looking for?
          stuff))))

  ;;;;;;;database procedures:

(define (bulk-insert destination . player-data)
  (apply insert-turn player-data))

(define (data-to-player data format)
  (if (null? format) '()
   (cons
    (cons (car format) (car data))
    (data-to-player (cdr data) (cdr format)))))

(define (player-to-data data)
  (define (make-level stuff)
    (map (lambda (n)
           (if (pair? n) (cdr n)
           n))stuff))
  (string-append* (cdr (append* (map (lambda (x) (list ", " x))
                                     (map make-into-escaped-string 
                                          (make-level data)))))))


(define (fetch-by-turn turn format)
  (define (parse data)
    (for-each (lambda (arg)
     (send-to-engine (data-to-player (vector->list arg) format)))
     data
     ))
  (parse
   (query-rows myconn (string-append 
                       "SELECT " basic-order
                       " FROM " (make-into-string tablename)
                       " WHERE turn = " (make-into-string turn) ))
   ))

(define (insert-turn dest data)
  (query-exec myconn (string-append
                      "INSERT INTO " (make-into-string dest)
                      " ( " (make-attrib-order data) " ) "
                      " VALUES ( " (player-to-data data) " )")
  ))