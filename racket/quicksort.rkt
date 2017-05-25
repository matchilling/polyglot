;; ----------------------------------------------------------------------------
;; Racket quick sort
;; ----------------------------------------------------------------------------
;;
;; Copyright (c) 2017 Mathias Schilling <m@matchilling>
;;
;; For the full copyright and license information, please view the LICENSE
;; file that was distributed with this source code.
;; ----------------------------------------------------------------------------
#lang racket

(provide (all-defined-out))

;; Create a list of random length `n` with a max value `max`
(define (randomlist n max)
  (cond
    [(= n 0) empty]
    [else (cons (+ 1 (random max)) (randomlist (- n 1) max))]))

;; Sort a list using quick sort algo
(define (quicksort < ls)
  (match ls
    ['() '()]
    [(cons x xs)
     (let-values ([(xs-gte xs-lt) (partition (curry < x) xs)])
       (append (quicksort < xs-lt)
               (list x)
               (quicksort < xs-gte)))]))
