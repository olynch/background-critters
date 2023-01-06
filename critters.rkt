#lang racket

(require 2htdp/image)
(require math/distributions)

(define squirrel (bitmap "squirrel4.png"))
(define squirrel-running (bitmap "squirrel_running4.png"))
(define chickadee (bitmap "chickadee4.png"))
(define cardinal (bitmap "cardinal4.png"))

(define background (bitmap "background.png"))

(struct segment (x0 y0 x1 y1))

(define pole-anchor (segment 350 620 500 620))
(define swings (segment 1350 620 1700 620))
(define climber-top (segment 680 290 1100 290))
(define fence-vertical (segment 2910 820 3300 1400))

(define (scale-seg seg amt)
  (match seg
    [(segment x0 y0 x1 y1) (segment (* x0 amt) (* y0 amt) (* x1 amt) (* y1 amt))]))

(define pole-anchor-4 (scale-seg pole-anchor 0.25))
(define swings-4 (scale-seg swings 0.25))
(define climber-top-4 (scale-seg climber-top 0.25))
(define fence-vertical-4 (scale-seg fence-vertical 0.25))

(define (place-img i1 seg s i2)
  (define x (+ (segment-x0 seg) (* s (- (segment-x1 seg) (segment-x0 seg)))))
  (define y (+ (segment-y0 seg) (* s (- (segment-y1 seg) (segment-y0 seg)))))
  (overlay/align/offset "left" "top" i1 (- x) (- y) i2))

(define critters (list squirrel squirrel-running chickadee cardinal))
(define segments (list pole-anchor swings climber-top fence-vertical))
(define start-state (list 0.5 0. 0.7 0.2))

(define (scene state)
  (foldl place-img
         background
         critters
         segments
         state))

(define noise (distribution-sample (normal-dist 0 0.05)))

(define (clamp x x-min x-max)
  (cond
    [(< x x-min) x-min]
    [(> x x-max) x-max]
    [#t x]))

(define (adjust state)
  (map (lambda (s) (clamp (+ s (noise)) 0 1)) state))

(define (run-background state)
  (define bg (scene state))
  (save-image bg "/home/o/.background-image")
  (system "feh --bg-scale ~/.background-image")
  (sleep 1)
  (run-background (adjust state)))

(run-background start-state)
