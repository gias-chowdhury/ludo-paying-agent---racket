#lang racket

(require racket/include
         2htdp/universe
         rnrs/enums-6)

(include "ludo-board.rkt")

(define-enumeration game-status (in-progress draw resume finished)
  status-set)

(define (get-next-player) (car playing-players))

(define (set-next-player current-player)
  (set! playing-players (append (remove current-player playing-players) (list current-player)))
  (set! message "Press P to play"))

(define (play-token player n)
  (cond ((ask player 'play-token n)
         (if (won? player)
             (begin
               (set! status (game-status finished))
               (set! message (string-append "Winner : " (ask player 'name))))
             (set-next-player player)))
        (else (set! message "Wrong token"))))

(define (won? player)
  (null? (filter (lambda (token) (not (equal? (ask token 'position) 500))) (ask player 'all-tokens))))

(define status (game-status in-progress))

(define (move-to-next-state current-state key)
  (let ((player (get-next-player)))
    (cond ((equal? status (game-status in-progress))
                  (cond ((key=? key "p") 
                         (ask player 'roll-dice 0)
                         (set! message "")
                         (cond ((and (> (ask player 'get-score) 6) (> (length (ask player 'get-tokens)) 0))
                                (ask player 'move-tokens-to-play)
                                (set! message "Select token to move"))
                               ((and (< (ask player 'get-score) 6) (not (ask player 'any-token-in-play?)))
                                (ask player 'reset-score)
                                (set-next-player player))
                               (else (set! message "Select token to move"))))
                        ((key=? key "1") (play-token player 1))
                        ((key=? key "2") (play-token player 2))
                        ((key=? key "3") (play-token player 3))
                        ((key=? key "4") (play-token player 4))
                        (else #f)))
          (else #t)) current-state))
  

(big-bang (position (/ WINDOW-SIZE 2) (+ (/ WINDOW-SIZE 2) 20))
          (on-key move-to-next-state)
          (to-draw draw-board-on-empty-scene))