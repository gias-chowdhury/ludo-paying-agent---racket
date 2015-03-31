;#lang racket
(require racket/include
         2htdp/image)

(include "config.rkt")

(define block-size 45)
(define WINDOW-SIZE 760)
(struct position (x y))
(define message "Press P to Play")


(define (draw-board board)
  (above (draw-top-pane)
         (beside (draw-start-block block-100 'orange)
                 (beside (draw-six-blocks above (find-blocks board '(11 10 9 8 7 6)))
                         (draw-six-blocks above (find-blocks board '(12 201 202 203 204 205)))
                         (draw-six-blocks above (find-blocks board '(13 14 15 16 17 18))))
                 (draw-start-block block-200 'red))
         (draw-middle-row)
         (beside (draw-start-block block-400 'blue)
                 (beside (draw-six-blocks above (find-blocks board '(44 43 42 41 40 39)))
                         (draw-six-blocks above (find-blocks board '(405 404 403 402 401 38)))
                         (draw-six-blocks above (find-blocks board '(32 33 34 35 36 37))))
                 (draw-start-block block-300 'green))))

(define (find-blocks board block-indices)
  (let ((blocks '()))
    (for-each (lambda (index) (set! blocks (append blocks (list (ask board 'get-block index))))) block-indices) blocks))
  
(define (draw-six-blocks proc blocks)
  (proc (draw-block (car blocks))
        (draw-block (car (cdr blocks)))
        (draw-block (car (cdr (cdr blocks))))
        (draw-block (car (cdr (cdr (cdr blocks)))))
        (draw-block (car (cdr (cdr (cdr (cdr blocks))))))
        (draw-block (car (cdr (cdr (cdr (cdr (cdr blocks)))))))))

(define (draw-home-block)
  (overlay (draw-home-block-tokens (ask big-block 'get-tokens)) (rectangle (* 3 block-size) (* 3 block-size) "solid" "black")))

(define (draw-top-pane)
  (beside (overlay (tokens-position (ask player1 'all-tokens)) (rectangle (/ WINDOW-SIZE 5) 20 "outline" "white"))
          (overlay (tokens-position (ask player2 'all-tokens)) (rectangle (/ WINDOW-SIZE 5) 20 "outline" "white"))
          (overlay (tokens-position (ask player3 'all-tokens)) (rectangle (/ WINDOW-SIZE 5) 20 "outline" "white"))
          (overlay (tokens-position (ask player4 'all-tokens)) (rectangle (/ WINDOW-SIZE 5) 20 "outline" "white"))
          (overlay (above (display-player-info) (text message 11 'black)) (rectangle (/ WINDOW-SIZE 5) 20 "outline" "white"))))

(define (display-player-info) 
  (text (string-append (string-append (string-append "Player : " (ask (car playing-players) 'name)) ", Score : ") (number->string (ask (car playing-players) 'get-score)))
        11 "black"))

(define (tokens-position tokens)
  (if (null? tokens)
      (text "" 11 "black")
      (beside (above (draw-token (car tokens)) (draw-number (ask (car tokens) 'position) "black" 14)) (tokens-position (cdr tokens)))))


(define (draw-start-block block color)
  (overlay (draw-tokens (ask block 'get-tokens) (quotient (* 6 block-size) 2)) (rectangle (* 6 block-size) (* 6 block-size) "solid" color)))

(define (draw-block block)
  (cond ((= (length (ask block 'get-tokens)) 1) 
         (overlay (text (number->string (ask (car (ask block 'get-tokens)) 'name)) 24 'white) 
                  (circle (/ block-size 2) "solid" (ask (car (ask block 'get-tokens)) 'color))
                  (rectangle block-size block-size "solid" (ask block 'color)))) 
        ((> (length (ask block 'get-tokens)) 1) 
         (overlay (text (number->string (length (ask block 'get-tokens))) 24 'black) (circle (/ block-size 2) "solid" "white")
                  (rectangle block-size block-size "solid" (ask block 'color))))
        ((ask block 'safe-block?)
         (overlay (text (number->string (ask block 'name)) 24 'black) (rectangle block-size block-size "solid" (ask block 'color))))
        (else (overlay (text (number->string (ask block 'name)) 24 'black) (rectangle block-size block-size "outline" (ask block 'color))))))

(define (draw-middle-row)
  (beside (above (draw-six-blocks beside  (find-blocks board '(52 1 2 3 4 5)))
                 (draw-six-blocks beside  (find-blocks board '(51 101 102 103 104 105)))
                 (draw-six-blocks beside  (find-blocks board '(50 49 48 47 46 45))))
          (overlay (text "Home" 24 'black) (draw-home-block))
          (above (draw-six-blocks beside  (find-blocks board '(19 20 21 22 23 24)))
                 (draw-six-blocks beside  (find-blocks board '(305 304 303 302 301 25)))
                 (draw-six-blocks beside  (find-blocks board '(31 30 29 28 27 26))))))

;Draw Token in Circle
(define (draw-token token)
    (draw-number (ask token 'name) (ask token 'color) 12))

(define (draw-number n color circle-radius)
    (overlay
     (text (number->string n) 14 "White")
     (circle circle-radius "solid" color)))


(define (place-and-turn token dial rad)
    (rotate rad
            (overlay/align "center" "top"
                           (draw-token token)
                           dial)))

(define (draw-tokens tokens circle-radius)
  (if (null? tokens)
      (circle circle-radius "solid" "white")
      (place-and-turn (car tokens) (draw-tokens (cdr tokens) circle-radius) 20)))

(define (draw-home-block-tokens tokens)
  (if (null? tokens)
      (circle (quotient (* 3 block-size) 2) "solid" "white")
      (place-and-turn (car tokens) (draw-home-block-tokens (cdr tokens)) 20)))


(define (draw-board-on-empty-scene current-state)
  (place-image (draw-board board)
               (position-x current-state)
               (position-y current-state)
               (empty-scene WINDOW-SIZE WINDOW-SIZE)))
