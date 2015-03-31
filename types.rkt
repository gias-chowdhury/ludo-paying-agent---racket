;#lang racket

(require racket/include)
(include "utility.rkt")

(define nil '())

(define (make-tokens count color)
  (let ((tokens '()))
    (for ([i count]) (set! tokens (cons (make-token (+ i 1) color) tokens)))
    tokens))

;Named object

(define (make-named-object name)
  (lambda (message) 
    (cond ((eq? message 'name) (lambda (self) name))
          (else (no-method name)))))

;; Definition of rule object
(define (make-bool-rule-object name)
  (lambda (message) 
    (cond ((eq? message 'can-i-add-token?) 
           (lambda (self block playing-player)
             (or (ask block 'safe-block?) (= (length (ask block 'playing-player-tokens playing-player)) 0))))
          
          ((eq? message 'can-i-remove-token?) 
           (lambda (self block token playing-player)
             (and (not (ask block 'safe-block?)) (not (equal? (ask token 'owner) playing-player)))))
          
          (else (no-method name)))))

;; Definition of start block object

(define (make-block name color safe-block?)
  (let ((named-obj  (make-named-object name))
        (next-block  #f)
        (tokens  '()))
    (lambda (message)
      (cond ((eq? message 'get-next-block)
             (lambda (self) next-block))
            
            ((eq? message 'set-next-block)
             (lambda (self block) (set! next-block block)))
            
            ((eq? message 'safe-block?)
             (lambda (self) safe-block?))
            
            ((eq? message 'color)
             (lambda (self) color))
            
            ((eq? message 'add-token)
             (lambda (self token)
               (if (ask self 'token-exists? token)
                   (display-message (list (ask token 'name) "is already at" name))
                   (set! tokens (cons token tokens)))))
            
            ((eq? message 'remove-token)
             (lambda (self token) 
               (set! tokens (remove token tokens))
               token))
            
            ((eq? message 'send-token-to-home) 
             (lambda (self token)
               (ask self 'remove-token token)
               (ask token 'set-position (ask (ask (ask token 'owner) 'start-block) 'name))
               (ask (ask token 'owner) 'token-removed token)))
            
            ((eq? message 'get-tokens)
             (lambda (self) tokens))
            
            ((eq? message 'set-tokens)
             (lambda (self lst) (set! tokens lst)))
            
            ((eq? message 'playing-player-tokens)
                  (lambda (self playing-player)
                      (filter (lambda (token) (if (eq? (ask token 'owner) playing-player) #t #f)) (ask self 'get-tokens))))
            
            ((eq? message 'other-players-tokens)
                  (lambda (self playing-player)
                      (filter (lambda (token) (if (not (eq? (ask token 'owner) playing-player)) #t #f)) (ask self 'get-tokens))))
            
            ((eq? message 'token-exists?)
             (lambda (self token) (memq token tokens)))
            
            ((eq? message 'other-players-token-exists?)
             (lambda (self playing-player) (> 0 (length (ask self 'other-players-tokens playing-player)))))
                       
            (else (get-method named-obj message))))))

;;; Definition of Treminal Block

(define (make-terminal-block name color)
  (let ((block-obj  (make-block name color #f))
        (right-block  #f))
    (lambda (message)
      (cond ((eq? message 'terminal-block?)
             (lambda (self) true))
            
            ((eq? message 'get-right-block)  
             (lambda (self) right-block))
            
            ((eq? message 'set-right-block)
             (lambda (self block) (set! right-block block)))
            
            (else (get-method block-obj message))))))

;;;Definition of Big Block

(define (make-big-block name color)
  (let ((block-obj  (make-block name color #t)))
    (lambda (message)
      (cond ((eq? message 'big-block?)
             (lambda (self) #t))
            
            ((eq? message 'get-next-block)
             (lambda (self) #f))
            
            ((eq? message 'set-next-block)
             (lambda (self block) #f))
            
            (else (get-method block-obj message))))))

(define (make-start-block name color)
  (let ((block-obj (make-block name color #t)))
    (lambda (message)
      (cond ((eq? message 'start-block?)
             (lambda (self) #t))
            (else (get-method block-obj message))))))

;;;;Definition of token

(define (make-token name color)
  (let ((named-obj  (make-named-object name))
        (position -1)
        (owner #f))
    (lambda (message)
      (cond ((eq? message 'token?)
             (lambda (self) true))
            
            ((eq? message 'position)
             (lambda (self) position))
            
            ((eq? message 'color)
             (lambda (self) color))
            
            ((eq? message 'set-position)
             (lambda (self location) (set! position location)))
            
            ((eq? message 'owner)
             (lambda (self) owner))
            
            ((eq? message 'set-owner)
             (lambda (self player) (set! owner player)))
            
            ((eq? message 'move)
                  (lambda (self step)
                    (set! position (+ position step))
                    position))
            
            ((eq? message 'in-play?)
             (lambda (self)
               (not (= 0 (remainder (ask self 'position) 100)))))
            
            ((eq? message 'install)
             (lambda (self)
               (ask owner 'add-token self)))
            
            (else (get-method named-obj message))))))

;;; Definition of board

(define (make-board name)
  (let ((named-obj  (make-named-object name))
        (players  '())
        (blocks  '()))
    (lambda (message)
      (cond ((eq? message 'add-block)
             (lambda (self block)
               (cond ((memq block blocks) (display-message (list (ask block 'name) "is already at" name)))
                     (else (set! blocks (cons block blocks))))))
            
            ((eq? message 'get-block)
                  (lambda (self block-name)
                    (car (filter (lambda (block) (if (equal? (ask block 'name) block-name) #t #f)) blocks))))
            
            ((eq? message 'all-blocks)
                  (lambda (self) blocks))
            
            ((eq? message 'get-big-block)
                  (lambda (self)
                    (car (filter (lambda (block) (not (no-method? (get-method block 'big-block?)))) blocks))))
            
            ((eq? message 'add-player)
             (lambda (self player)
               (cond ((memq player players)
                      (display-message (list (ask player 'name) "is already at" name)))
                     (else (set! players (cons player players))))))
            
            ((eq? message 'players)
                  (lambda (self)
                    players))
            
            ((eq? message 'all-tokens)
                  (lambda (self)
                    (let ((tokens '()))
                      (for-each (lambda (block) (set! tokens (append tokens (ask block 'get-tokens)))) blocks)
                      tokens)))
            
            ((eq? message 'all-playing-tokens)
                  (lambda (self)
                    (filter (lambda (token) (ask token 'in-play?)) (ask self 'all-tokens))))
                        
            ((eq? message 'player-tokens)
                  (lambda (self playing-player)
                      (filter (lambda (token) (if (eq? (ask token 'owner) playing-player) #t #f)) (ask self 'all-tokens))))
            
            ((eq? message 'player-playing-tokens)
                  (lambda (self playing-player)
                      (filter (lambda (token) (if (eq? (ask token 'owner) playing-player) #t #f)) (ask self 'all-playing-tokens))))
            
            ((eq? message 'other-players-tokens)
                  (lambda (self playing-player)
                      (filter (lambda (token) (if (not (eq? (ask token 'owner) playing-player)) #t #f)) (ask self 'all-tokens))))
            
            (else (get-method named-obj message))))))

;;; ----------------------------------------------------------------------------
;;; Implementation of player

(define (make-player name board start-block end-block)
  (let ((named-obj  (make-named-object name))
        (rule-obj (make-bool-rule-object 'rule))
        (score 0)
        (playing-status #t)
        (previous-draw 0))
    
    (lambda (message)
      (cond ((eq? message 'player?) (lambda (self) true))
            ((eq? message 'playing-tokens) (lambda (self) (ask board 'player-playing-tokens self)))
            ((eq? message 'start-block) (lambda (self) start-block))
            ((eq? message 'end-block) (lambda (self) end-block))
            ((eq? message 'get-score) (lambda (self) score))
            ((eq? message 'reset-score) (lambda (self) (set! score 0)))
            ((eq? message 'any-token-in-play?) (lambda (self) (not (null? (ask self 'playing-tokens)))))
            ((eq? message 'in-play?) (lambda (self) playing-status))              
            ((eq? message 'change-playing-status) (lambda (self status) (set! playing-status status)))
            ((eq? message 'get-tokens) (lambda (self) (ask start-block 'get-tokens)))
            ((eq? message 'set-tokens) (lambda (self tokens)
                                         (for-each (lambda (token)
                                                     (ask token 'set-position (ask start-block 'name))
                                                     (ask token 'set-owner self)
                                                     (ask token 'set-owner self)) tokens)
                                         (ask start-block 'set-tokens tokens)))
            
            ((eq? message 'all-tokens) (lambda (self) (ask board 'player-tokens self)))
            
            ((eq? message 'add-playing-token) 
             (lambda (self)
               (let* ((my-tokens (ask self 'get-tokens))
                     (token-to-add (list-ref my-tokens (random (length my-tokens)))))
                  (cond ((not (null? my-tokens))
                         (ask token-to-add 'set-position (ask (ask start-block 'get-next-block) 'name)) (ask (ask start-block 'get-next-block) 'add-token token-to-add)
                         (ask self 'playing-token-added token-to-add))
                        (else #f)))))
                        
            ((eq? message 'playing-token-added) 
             (lambda (self token)
               (ask self 'set-tokens (remove token (ask self 'get-tokens)))))
                        
            ((eq? message 'token-removed) 
             (lambda (self token)
               (ask self 'set-tokens (cons token (ask self 'get-tokens)))))
            
            ((eq? message 'get-next-block) 
             (lambda (self block step)
               (let ((next-block block))
                 (for ([i step]) 
                   #:break (not next-block)
                   (begin
                     (cond ((eq? next-block end-block) (set! next-block (ask next-block 'get-right-block)))
                           (else (set! next-block (ask next-block 'get-next-block))))))
                 next-block)))
            
            ((eq? message 'move-token) 
             (lambda (self token step)
               (let* ((current-block (ask board 'get-block (ask token 'position)))
                      (next-block (ask self 'get-next-block current-block step)))
                 (if (and next-block (ask rule-obj 'can-i-add-token? next-block self))
                     (begin
                       (if (and (ask next-block 'other-players-token-exists? self)
                                (ask rule-obj 'can-i-remove-token? next-block (car (ask next-block 'other-players-tokens)) self))
                           (ask next-block 'send-token-to-home (car (ask next-block 'other-players-tokens)))
                           #f)
                       (ask token 'set-position (ask next-block 'name))
                       (ask next-block 'add-token token)
                       (ask current-block 'remove-token token))
                     #f))))
            
            ((eq? message 'get-token) 
             (lambda (self)
               (let ((playing-tokens (ask board 'player-playing-tokens self)))
                 (if (equal? playing-tokens '())
                     #f
                     (list-ref playing-tokens (random (length playing-tokens)))))))
            
            ((eq? message 'select-token) 
             (lambda (self token-name)
               (car (filter (lambda (token) (equal? (ask token 'name) token-name)) (append (ask self 'playing-tokens) (ask self 'get-tokens))))))
            
            ((eq? message 'select-token-to-play) 
             (lambda (self token-name)
               (let ((tokens (filter (lambda (token) (equal? (ask token 'name) token-name)) (ask self 'playing-tokens))))
                 (if (null? tokens)
                     #f
                     (car tokens)))))

            ((eq? message 'display-tokens) 
             (lambda (self) 
               (for-each (lambda (token) 
                           (newline)
                           (display (list 'Token (ask token 'name) "at " (ask token 'position))))
                         (ask self 'all-tokens))))
            
            ((eq? message 'draw-dice)
                  (lambda (self)
                    (let ((draw (+ (random 6) 1)))
                      (if (not (= previous-draw 6)) (set! score 0) #f)
                      (set! previous-draw draw)
                      (set! score (remainder (+ score draw) 18))
                      draw)))
            
            ((eq? message 'roll-dice) 
             (lambda (self value) 
               (let ((draw (ask self 'draw-dice)))
                       (if (= draw 6)
                           (ask self 'roll-dice (+ draw value))
                           (remainder (+ draw value) 18)))))
            
            ((eq? message 'move-tokens-to-play)
             (lambda (self)
               (let ((tokens-to-add (length (ask self 'get-tokens)))
                     (count-six (quotient score 6)))
                 (cond ((and (> tokens-to-add 0) (> count-six tokens-to-add))
                        (for ([i (in-range 0 tokens-to-add)]) (ask self 'add-playing-token) (set! score (- score 6))))
                       ((and (> tokens-to-add 0) (< count-six tokens-to-add))
                        (for ([i (in-range 0 count-six)]) (ask self 'add-playing-token) (set! score (- score 6))))
                       ((and (> tokens-to-add 0) (= count-six tokens-to-add))
                        (for ([i (in-range 0 tokens-to-add)]) (ask self 'add-playing-token) (set! score (- score 6))))
                       (else #f)))))
            
            ((eq? message 'play-token)
             (lambda (self token-name)
               (let ((token-to-play (ask self 'select-token-to-play token-name))
                     (playing-tokens (ask self 'playing-tokens)))
                     
                 (cond ((and (> (remainder score 6) 0) token-to-play)
                        (ask self 'move-token token-to-play score) (set! score 0) #t)
                       (else #f)))))
            
            (else (get-method named-obj message))))))

;;; ----------------------------------------------------------------------------
;;; Implementation of agent

(define (make-agent name board start-block end-block)
  (let ((player  (make-player name board start-block end-block)))
    (lambda (message)
      (cond ((eq? message 'agent?) (lambda (self) true))
            ((eq? message 'get-token) 
             (lambda (self)
               (let ((playing-tokens (ask board 'player-playing-tokens self)))
                 (if (equal? playing-tokens '())
                     #f
                     (list-ref playing-tokens (random (length playing-tokens)))))))
            
            ((eq? message 'play)
             (lambda (self)
               (ask self 'roll-dice 0)
               (display-message (list 'Score (ask self 'get-score)))
               (newline)
               (if (> (ask self 'get-score) 5)
                   (ask self 'move-tokens-to-play)
                   #f)
               (if (ask self 'get-token)
                   (ask self 'play-token (ask (ask self 'get-token) 'name))
                   (display-message (list "No token to play!")))))
          
            (else (get-method player message))))))


