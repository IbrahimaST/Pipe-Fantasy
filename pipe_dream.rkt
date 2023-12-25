;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw11_finale) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/image)
(require 2htdp/universe)
(require racket/bool)


;;;;;;;;;;;;;;;;;;;;;;
;; DATA DEFINITIONS ;;
;;;;;;;;;;;;;;;;;;;;;;



(define-struct pipe [top bot left right])
;; A Pipe is a (make-pipe Boolean Boolean Boolean Boolean)
;; Interpretation: a pipe with openings in the given directions. A #true for 
;; one of top, bot, left, right indicates an opening in that direction.
;; A starting pipe only has an opening in one direction

(define PIPE-TB (make-pipe #true #true #false #false))
(define PIPE-TL (make-pipe #true #false #true #false))
(define PIPE-TR (make-pipe #true #false #false #true))
(define PIPE-LR (make-pipe #false #false #true #true))
(define PIPE-BL (make-pipe #false #true #true #false))
(define PIPE-BR (make-pipe #false #true #false #true))
(define PIPE-CROSS (make-pipe #true #true #true #true))

(define PIPE-STARTER-TOP (make-pipe #t #f #f #f))
(define PIPE-STARTER-BOTTOM (make-pipe #f #t #f #f))
(define PIPE-STARTER-LEFT (make-pipe #f #f #t #f))
(define PIPE-STARTER-RIGHT (make-pipe #f #f #f #t))

(define ALL-PIPES (list PIPE-TB PIPE-TL PIPE-TR PIPE-LR PIPE-BL PIPE-BR PIPE-CROSS))

(define (pipe-temp p)
  (... (pipe-top p) ...
       (pipe-bot p) ...
       (pipe-left p) ...
       (pipe-right p) ...))



(define-struct coord-of-pipe [pipe row col])
;; A PipeLocation is a (make-coord-of-pipe Pipe Number Number)
;; Interpretation: A Pipe with its given coordinates

(define PIPE-TOP-BOTTOM-1-2 (make-coord-of-pipe PIPE-TB 1 2))
(define PIPE-TOP-BOTTOM-2-2 (make-coord-of-pipe PIPE-TB 2 2))
(define PIPE-TOP-LEFT-3-2 (make-coord-of-pipe PIPE-TL 3 2))
(define PIPE-LEFT-RIGHT-2-0 (make-coord-of-pipe PIPE-LR 2 0))
(define PIPE-STARTER-TOP-2-1 (make-coord-of-pipe PIPE-STARTER-TOP 2 1))
(define PIPE-STARTER-BOTTOM-2-1 (make-coord-of-pipe PIPE-STARTER-BOTTOM 2 1))
(define PIPE-STARTER-LEFT-2-1 (make-coord-of-pipe PIPE-STARTER-LEFT 2 1))
(define PIPE-STARTER-RIGHT-2-1 (make-coord-of-pipe PIPE-STARTER-RIGHT 2 1))
(define PIPE-BOTTOM-RIGHT-1-1 (make-coord-of-pipe PIPE-BR 1 1))
(define PIPE-TOP-BOTTOM-1-1 (make-coord-of-pipe PIPE-TB 1 1))
(define (pipe-loc-temp loc-pipe)
  (... (pipe-temp (coord-of-pipe-pipe loc-pipe)) ...
       (coord-of-pipe-row loc-pipe) ...
       (coord-of-pipe-col loc-pipe) ...))



(define-struct grid [size pipes])
;; A Grid is a (make-grid Number [ListOf PipeLocation])
;; Interpretation: A grid of size n * n with pipes at given positions

(define STARTING-GRID (make-grid 7 (list)))
(define GRID-7-1 (make-grid 7 (list PIPE-TOP-BOTTOM-1-2)))
(define GRID-7-2 (make-grid 7 (list PIPE-TOP-LEFT-3-2 PIPE-TOP-BOTTOM-1-2)))
(define GRID-7-3 (make-grid 7 (list PIPE-LEFT-RIGHT-2-0 PIPE-TOP-LEFT-3-2 PIPE-TOP-BOTTOM-1-2)))
(define GRID-7-4 (make-grid 7 (list PIPE-TOP-BOTTOM-1-2 PIPE-TOP-BOTTOM-2-2)))
(define GRID-7-5 (make-grid 7 (list PIPE-STARTER-TOP-2-1 PIPE-BOTTOM-RIGHT-1-1)))
(define GRID-7-6 (make-grid 7 (list PIPE-STARTER-TOP-2-1)))

(define (grid-temp grid)
  (... (grid-size grid) ...
       (cond [(empty? (grid-pipes grid)) ...]
             [(cons? (grid-pipes grid)) ...]) ...))



;; A Direction is one of
;; - "UP"
;; - "DOWN"
;; - "LEFT"
;; - "RIGHT"
;; A Direction in the grid

(define UP "UP")
(define DOWN "DOWN")
(define LEFT "LEFT")
(define RIGHT "RIGHT")

(define (direction-temp d)
  (cond [(string=? d UP) ...]
        [(string=? d DOWN) ...]
        [(string=? d LEFT) ...]
        [(string=? d RIGHT) ...]))



(define-struct gooflow [next-direction path])
;; A GooFlow is a (make-gooflow [ListOf String][ListOf PipeLocations]
;; Interpretation the direction the goo is flowing where next-direction is a list of strings
;; that is the direction of goo flow, and path is the list of PipeLocations that is a list of make-coord-of-pipe

(define GOOFLOW-1 (make-gooflow (list "UP") (list PIPE-STARTER-TOP-2-1)))
(define GOOFLOW-2 (make-gooflow (list "DOWN") (list PIPE-STARTER-BOTTOM-2-1)))
(define GOOFLOW-3 (make-gooflow (list "LEFT") (list PIPE-STARTER-LEFT-2-1)))
(define GOOFLOW-4 (make-gooflow (list "RIGHT") (list PIPE-STARTER-RIGHT-2-1)))

(define (gooflow-temp gf)
  (... (gooflow-next-direction gf) ...
       (cond [(empty? (gooflow-path gf)) ...]
             [(cons? (gooflow-path gf)) ...]) ...))



(define-struct gamestate [grid incoming tile-width pipe-width starting-pipe gooflow replaced time])
;; A GameState is a (make-gamestate Grid [ListOf Pipe] Integer Integer PipeLocation GooFlow number)
;; Interpretation: A grid that contains a list of incoming pipes

(define GS-INIT (make-gamestate STARTING-GRID ALL-PIPES 60 20 PIPE-STARTER-TOP-2-1 GOOFLOW-1 0 0))
(define GS-ST-2-1 (make-gamestate GRID-7-6 ALL-PIPES 60 20 PIPE-STARTER-TOP-2-1 GOOFLOW-1 0 0))

(define (gamestate-temp gs)
  (... (grid-temp (gamestate-grid gs)) ...
       (gamestate-tile-width gs) ...
       (gamestate-pipe-width gs) ...
       (gamestate-starting-pipe gs) ...
       (gamestate-replaced gs) ...
       (gamestate-time gs) ...
       (cond [(empty? (gamestate-incoming gs)) ...]
             [(cons? (gamestate-incoming gs)) ...]) ...))



;; init-starting-pipe : Direction -> Pipe
;; Returns a Starting Pipe in a given direction
(define (init-starting-pipe direction)
  (cond [(string=? direction UP) PIPE-STARTER-TOP]
        [(string=? direction DOWN) PIPE-STARTER-BOTTOM]
        [(string=? direction LEFT) PIPE-STARTER-LEFT]
        [(string=? direction RIGHT) PIPE-STARTER-RIGHT]))

(check-expect (init-starting-pipe DOWN) PIPE-STARTER-BOTTOM)
(check-expect (init-starting-pipe UP) PIPE-STARTER-TOP)
(check-expect (init-starting-pipe LEFT) PIPE-STARTER-LEFT)
(check-expect (init-starting-pipe RIGHT) PIPE-STARTER-RIGHT)

;; pipe->image: Pipe Integer Integer Boolean -> Image
;; Draws the given pipe on a square tile with length tile-side-length. The width
;; of the pipe is pipe-width. Pipe-width should be less than tile-side-length
(define (pipe->image pipe tile-side-length pipe-width filled? direction)
  (local [;; Angles for pipes
          (define TOP-GEOMETRIC-ANGLE -90)
          (define BOTTOM-GEOMETRIC-ANGLE 90)
          (define LEFT-GEOMETRIC-ANGLE 0)
          (define RIGHT-GEOMETRIC-ANGLE 180)

          ;; Image Parts
          (define SEGMENT-OF-PIPE (rectangle (+ pipe-width (/ (- tile-side-length pipe-width) 2)) pipe-width "solid" (if filled?
                                                                                                                         "green"
                                                                                                                         "black")))
          (define INTERVAL (rectangle (/ (- tile-side-length pipe-width) 2) pipe-width
                                      "solid" "transparent"))

          ;; pipe-seg->image: Integer -> Image
          ;; Gives us a segment of a pipe from a tile width and a pipe width.
          ;; The angle is the angle of rotation starting left
          (define (seg-of-pipe angle)
            (rotate angle (beside SEGMENT-OF-PIPE
                                  INTERVAL)))]
    (if (and (pipe-top pipe) (pipe-bot pipe) (pipe-left pipe) (pipe-right pipe) filled?)
        (if (string=? direction "ALL")
            (overlay
             (rotate TOP-GEOMETRIC-ANGLE (beside (rectangle (+ pipe-width (/ (- tile-side-length pipe-width) 2)) pipe-width "solid" "green") INTERVAL))
             (rotate BOTTOM-GEOMETRIC-ANGLE (beside (rectangle (+ pipe-width (/ (- tile-side-length pipe-width) 2)) pipe-width "solid" "green") INTERVAL))
             (rotate LEFT-GEOMETRIC-ANGLE (beside (rectangle (+ pipe-width (/ (- tile-side-length pipe-width) 2)) pipe-width "solid" "green") INTERVAL))
             (rotate RIGHT-GEOMETRIC-ANGLE (beside (rectangle (+ pipe-width (/ (- tile-side-length pipe-width) 2)) pipe-width "solid" "green") INTERVAL))
             (square tile-side-length "solid" "gray"))
            
            (if (or (string=? direction "UP") (string=? direction "DOWN"))
                (overlay
                 (rotate TOP-GEOMETRIC-ANGLE (beside (rectangle (+ pipe-width (/ (- tile-side-length pipe-width) 2)) pipe-width "solid" "green") INTERVAL))
                 (rotate BOTTOM-GEOMETRIC-ANGLE (beside (rectangle (+ pipe-width (/ (- tile-side-length pipe-width) 2)) pipe-width "solid" "green") INTERVAL))
                 (rotate LEFT-GEOMETRIC-ANGLE (beside (rectangle (+ pipe-width (/ (- tile-side-length pipe-width) 2)) pipe-width "solid" "black") INTERVAL))
                 (rotate RIGHT-GEOMETRIC-ANGLE (beside (rectangle (+ pipe-width (/ (- tile-side-length pipe-width) 2)) pipe-width "solid" "black") INTERVAL))
                 (square tile-side-length "solid" "gray"))
                (overlay
                 (rotate LEFT-GEOMETRIC-ANGLE (beside (rectangle (+ pipe-width (/ (- tile-side-length pipe-width) 2)) pipe-width "solid" "green") INTERVAL))
                 (rotate RIGHT-GEOMETRIC-ANGLE (beside (rectangle (+ pipe-width (/ (- tile-side-length pipe-width) 2)) pipe-width "solid" "green") INTERVAL))
                 (rotate TOP-GEOMETRIC-ANGLE (beside (rectangle (+ pipe-width (/ (- tile-side-length pipe-width) 2)) pipe-width "solid" "black") INTERVAL))
                 (rotate BOTTOM-GEOMETRIC-ANGLE (beside (rectangle (+ pipe-width (/ (- tile-side-length pipe-width) 2)) pipe-width "solid" "black") INTERVAL)) 
                 (square tile-side-length "solid" "gray"))))
                        
             
        (overlay      
         (if (pipe-top pipe)
             (seg-of-pipe TOP-GEOMETRIC-ANGLE)
             empty-image)
         (if (pipe-bot pipe)
             (seg-of-pipe BOTTOM-GEOMETRIC-ANGLE)
             empty-image)
         (if (pipe-left pipe)
             (seg-of-pipe LEFT-GEOMETRIC-ANGLE)
             empty-image)
         (if (pipe-right pipe)
             (seg-of-pipe RIGHT-GEOMETRIC-ANGLE)
             empty-image)
         (square tile-side-length "solid" "gray")))))

;; list-of-pipes->image: [ListOf Pipe] Integer Integer Integer String -> Image
;; Draws a list of n pipes of size n with tile size n
(define (list-of-pipes->image lop tile-width pipe-width length direction)
  (cond [(and (empty? lop) (> length 0)) (beside (square tile-width "solid" "black") (list-of-pipes->image lop tile-width pipe-width
                                                                                                           (- length 1) direction))]
        [(= length 0) empty-image]
        [(cons? lop) (beside (pipe->image (first lop) tile-width pipe-width #false direction)
                             (list-of-pipes->image (rest lop) tile-width pipe-width (- length 1) direction))]))
  



;; place-pipe: Grid Pipe Integer Integer -> Grid
;; Places the pipe on the grid at the given row and column. We assume that the
;; row and column are valid positions on the grid.
(define (place-pipe grid pipe row col)
  (make-grid (grid-size grid)
             (append (list (make-coord-of-pipe pipe row col))
                     (filter (lambda (x) (not (and (= row (coord-of-pipe-row x))
                                                   (= col (coord-of-pipe-col x))))) (grid-pipes grid)))))

(check-expect (place-pipe STARTING-GRID PIPE-TB 1 2) GRID-7-1)
(check-expect (place-pipe GRID-7-1 PIPE-TL 3 2) GRID-7-2)
(check-expect (place-pipe GRID-7-2 PIPE-LR 2 0) GRID-7-3)
;; place-pipe-on-click : GameState Integer Integer MouseEvent -> GameState
;; If the user clicks on a tile and there are incoming pipes available, places
;; the next incoming pipe on that tile. If no pipes are available, does nothing.
(define (place-pipe-on-click gs x y event)
  (local [(define COL (floor (/ y (gamestate-tile-width gs))))
          (define ROW (floor (/ x (gamestate-tile-width gs))))]
    (if (and (string=? event "button-up")
             (< (max ROW COL) (grid-size (gamestate-grid gs)))
             (not (and (= ROW ( + (coord-of-pipe-row (gamestate-starting-pipe gs))1))
                       (= COL (+ (coord-of-pipe-col (gamestate-starting-pipe gs))1)))) (boolean? (pipe-at (make-grid (grid-size (gamestate-grid gs)) (gooflow-path(gamestate-gooflow gs))) COL ROW)))
        
        (if (empty? (gamestate-incoming gs)) (make-gamestate (gamestate-grid gs)  (gamestate-incoming gs) (gamestate-tile-width gs) (gamestate-pipe-width gs)  (gamestate-starting-pipe gs)
                                                             (grid-goo-propagate (gamestate-gooflow gs)  (gamestate-grid gs)) (gamestate-replaced gs) (gamestate-time gs))
            (make-gamestate (place-pipe (gamestate-grid gs) (first (gamestate-incoming gs)) (floor (/ y (gamestate-tile-width gs)))(floor (/ x (gamestate-tile-width gs))))
                                         (rest (gamestate-incoming gs)) (gamestate-tile-width gs) (gamestate-pipe-width gs) (gamestate-starting-pipe gs) (gamestate-gooflow gs)
                                         (if (pipe? (pipe-at (gamestate-grid gs)
                                                             COL
                                                             ROW))
                                             (add1 (gamestate-replaced gs))
                                             (gamestate-replaced gs))
                                         (gamestate-time gs)))
                                             
        gs)))

;; pipe-at: Grid Integer Integer -> [Optional Pipe]
;; Produces the pipe at the given row and column, or #false if that position is
;; is blank. We assume that the row and column are valid positions on the grid.
(define (pipe-at grid row col)
  (cond [(empty? (grid-pipes grid)) #false]
        [(cons? (grid-pipes grid)) (if (and (= (coord-of-pipe-row (first (grid-pipes grid)))
                                               row) (= (coord-of-pipe-col (first (grid-pipes grid))) col))
                                       (coord-of-pipe-pipe (first (grid-pipes grid))) (pipe-at (make-grid (grid-size grid)
                                                                                                          (rest (grid-pipes grid))) row col))]))
(check-expect (pipe-at GRID-7-1 1 2) PIPE-TB)
(check-expect (pipe-at GRID-7-2 2 0) #false)
(check-expect (pipe-at GRID-7-3 3 2) PIPE-TL)

;; pipe-at-goo: gooflow Integer Integer -> boolean
;; If a pipe exists at the location in gooflow returns #t, #f otherwise
(define (pipe-at-goo gf row col)
  (cond [(empty? (gooflow-path gf)) #false]
        [(cons? (gooflow-path gf)) (if (and (= (coord-of-pipe-row (first (gooflow-path gf)))
                                               row) (= (coord-of-pipe-col (first (gooflow-path gf))) col))
                                       #true
                                       (pipe-at-goo (make-gooflow (rest (gooflow-path gf)) (gooflow-next-direction gf)) row col)
                                       )]))

;; next-pipe : PipeLocation Direction Grid -> [Optional PipeLocation]
;; Returns the next pipe in the grid
(define (pipe-after ploc direc grid)
  (local [(define ROW (+ (coord-of-pipe-row ploc) (cond [(string=? (first direc) UP) -1] [(string=? (first direc) DOWN) 1]
                                                        [else 0])))
          (define COL (+ (coord-of-pipe-col ploc) (cond [(string=? (first direc) LEFT) -1] [(string=? (first direc) RIGHT) 1]
                                                        [else 0])))

          (define NEXT-PIPE (pipe-at grid ROW COL))]
    (cond [(boolean? NEXT-PIPE) #false]
          [(pipe? NEXT-PIPE) (make-coord-of-pipe NEXT-PIPE ROW COL)])))

(check-expect (pipe-after PIPE-STARTER-TOP-2-1 (list "UP") GRID-7-5) PIPE-BOTTOM-RIGHT-1-1)
(check-expect (pipe-after PIPE-TOP-BOTTOM-1-2 (list "DOWN") GRID-7-4) PIPE-TOP-BOTTOM-2-2)
(check-expect (pipe-after PIPE-TOP-BOTTOM-1-2 (list "UP") GRID-7-4) #f)

;; flow? : Direction Pipe -> Boolean
;; Returns true if pipe opening at direction
(define (flow? d p)
  (cond
    [(string=? (first d) UP) (pipe-bot p)]
    [(string=? (first d) DOWN) (pipe-top p)]
    [(string=? (first d) LEFT) (pipe-right p)]
    [(string=? (first d) RIGHT) (pipe-left p)]))

(check-expect (flow? (list "LEFT") PIPE-LR) #t)
(check-expect (flow? (list "LEFT") PIPE-TL) #f)
(check-expect (flow? (list "DOWN") PIPE-TR) #t)


;; new-direction : LoS Pipe -> Direction
;; Return the direction through pipe
(define (new-direction dir pipe)
  (cond [(and (pipe-top pipe)
              (pipe-bot pipe)
              (pipe-left pipe)
              (pipe-right pipe)) (first dir)]
        [(and (pipe-top pipe) (not (string=? (first dir) DOWN))) UP]
        [(and (pipe-bot pipe) (not (string=? (first dir) UP))) DOWN]
        [(and (pipe-left pipe) (not (string=? (first dir) RIGHT))) LEFT]
        [(and (pipe-right pipe) (not (string=? (first dir) LEFT))) RIGHT]))

(check-expect (new-direction (list "DOWN") PIPE-CROSS) DOWN)
(check-expect (new-direction (list "DOWN") PIPE-TL) LEFT)
(check-expect (new-direction (list "UP") PIPE-TB) UP)
(check-expect (new-direction (list "UP") PIPE-CROSS) UP)

;; grid-goo-propagate : GooFlow Grid -> GooFlow
;; Moves the goo flow one tile forward
(define (grid-goo-propagate gf grid)
  (local [(define NEXT-PIPE (pipe-after (first (gooflow-path gf))
                                        (gooflow-next-direction gf)
                                        grid))]
    (cond [(boolean? NEXT-PIPE) gf]
          [(coord-of-pipe? NEXT-PIPE) (if (flow? (gooflow-next-direction gf)(coord-of-pipe-pipe NEXT-PIPE))
                                          (make-gooflow (cons (new-direction (gooflow-next-direction gf) (coord-of-pipe-pipe NEXT-PIPE)) (gooflow-next-direction gf))
                                                        (cons NEXT-PIPE (gooflow-path gf)))
                                          gf)])))
  
(check-expect (grid-goo-propagate GOOFLOW-1 (make-grid 7 (list PIPE-TOP-BOTTOM-1-1))) (make-gooflow (list UP UP) (list PIPE-TOP-BOTTOM-1-1  PIPE-STARTER-TOP-2-1)))
(check-expect (grid-goo-propagate GOOFLOW-1 GRID-7-6) GOOFLOW-1)
;; grid->image: Grid Integer Integer -> Image
;; Draws the grid of pipes. Every tile should be a square with side length
;; tile-side-length and every pipe should have width pipe-width.
(define (grid->image grid tile-side-length pipe-width gooflow)
  (local [
          (define (draw-n-imgs reps image)
            (cond [(= reps 0) empty-image]
                  [(not (= reps 0)) (beside image (draw-n-imgs (- reps 1) image))]))
          
          ;; draw-grid Num -> Image
          ;; Draws a grid of size n * n
          (define (draw-grid size)
            (draw-n-imgs size
                         (rotate 90
                                 (draw-n-imgs size
                                              (square tile-side-length "outline" "black")))))]
    
    (cond
      [(empty? (grid-pipes grid)) (draw-grid (grid-size grid))]
      [(cons? (grid-pipes grid))
       (underlay/xy (grid->image (make-grid (grid-size grid)
                                            (rest (grid-pipes grid)))
                                 tile-side-length
                                 pipe-width
                                 gooflow)
                    (* (coord-of-pipe-col (first (grid-pipes grid))) tile-side-length)
                    (* (coord-of-pipe-row (first (grid-pipes grid))) tile-side-length)
                    (pipe->image (coord-of-pipe-pipe (first (grid-pipes grid)))
                                 tile-side-length
                                 pipe-width
                                 (ormap (lambda (x) (and (= (coord-of-pipe-row (first (grid-pipes grid)))
                                                            (coord-of-pipe-row x))
                                                         (= (coord-of-pipe-col (first (grid-pipes grid)))
                                                            (coord-of-pipe-col x))))
                                        (gooflow-path gooflow)) (grid-direction-helper (first (grid-pipes grid)) gooflow)))])))

;; grid-direction-helper : Pipe GooFlow --> String
;; Finds the direction of the pipe that was found in the gooflow
(define (grid-direction-helper p gf)
  (local[
         (define (goo-acc c lop)           
           (cond
             [(empty? lop) ""]
             [(cons? lop) (if (pipecoords=? (first lop) p)
                              (if (hasgoo? p (rest lop))
                                  "ALL"
                                  (list-ref (gooflow-next-direction gf) c))
                              (goo-acc (add1 c) (rest lop)))]))]
    (goo-acc 0 (gooflow-path gf))))

;; hasgoo? : PipeCoords [List-of PipeCoords] -> Boolean
;; Determines if the given pipe is in the gooflow/has goo
(define (hasgoo? pc lopc)
  (cond
    [(empty? lopc) #false]
    [(list? lopc)
     (if (and (pipe=? (coord-of-pipe-pipe pc) (coord-of-pipe-pipe (first lopc)))
              (= (coord-of-pipe-col pc) (coord-of-pipe-col (first lopc)))
              (= (coord-of-pipe-row pc) (coord-of-pipe-row (first lopc))))
         #t
         (hasgoo? pc (rest lopc)))]))

    
;; pipe=? : pipe pipe --> Boolean
;; Determines if the two given pipes are equal
(define (pipe=? p0 p1)
  (and (not (xor (pipe-top p0) (pipe-top p1)))
       (not (xor (pipe-bot p0) (pipe-bot p1)))
       (not (xor (pipe-left p0) (pipe-left p1)))
       (not (xor (pipe-right p0) (pipe-right p1)))))

(define (pipecoords=? p0 p1)
  (and (pipe=? (coord-of-pipe-pipe p0) (coord-of-pipe-pipe p0))
       (= (coord-of-pipe-col p0) (coord-of-pipe-col p1))
       (= (coord-of-pipe-row p0) (coord-of-pipe-row p1))))

 
;; gamestate->image : GameState -> Image
;; Draws the gamestate
(define (gamestate->image gs)
  (above (grid->image (gamestate-grid gs) (gamestate-tile-width gs) (gamestate-pipe-width gs) (gamestate-gooflow gs))
         (list-of-pipes->image (gamestate-incoming gs) (gamestate-tile-width gs) (gamestate-pipe-width gs) (grid-size (gamestate-grid gs)) (gooflow-next-direction (gamestate-gooflow gs)))
         (text (number->string (get-score gs)) 20 "black")))
  
          



;; gamestate-init : Number Number Number Direction [ListOf Pipe] -> GameState
;; A GameState is Initialized
(define (gamestate-init dim x y dir pipes)
  (local [(define STARTING-PIPE (make-coord-of-pipe (init-starting-pipe dir) x y))]
    (make-gamestate (make-grid dim (list STARTING-PIPE))
                    pipes
                    60
                    20
                    STARTING-PIPE (make-gooflow (list dir) (list STARTING-PIPE)) 0 0)))

(check-expect (gamestate-init 7 2 1 UP ALL-PIPES) GS-ST-2-1)

;; autogoo : Gamestate --> Gamestate
;; Advances the goo automatically
(define (autogoo gs)  
      (make-gamestate (gamestate-grid gs) (gamestate-incoming gs) (gamestate-tile-width gs) (gamestate-pipe-width gs) (gamestate-starting-pipe gs)
                      (if (and (> (gamestate-time gs) 140) (= (modulo (gamestate-time gs) 28) 0))
                          (grid-goo-propagate (gamestate-gooflow gs) (gamestate-grid gs))
                          (gamestate-gooflow gs))
                          (gamestate-replaced gs) (add1 (gamestate-time gs))))
      
 

;; pipe-fantasy: GameState -> GameState
(define (pipe-fantasy initial-game-state)
  (big-bang initial-game-state
    [to-draw gamestate->image]
    [on-tick autogoo]
    [on-mouse place-pipe-on-click]))

(gamestate-init 7 2 3 UP ALL-PIPES)
(gamestate-init 5 2 1 UP ALL-PIPES)

;; get-score : Gamestate --> Integer
;; Computes the current score of the game
(define (get-score gs)
  (* 50 (- (length (gooflow-path (gamestate-gooflow gs))) (gamestate-replaced gs)))) 

;; EXAMPLES:
(define GS_EXAMPLE1 (gamestate-init 5 2 2 UP (list PIPE-TB PIPE-TR PIPE-BL PIPE-TR PIPE-CROSS PIPE-BL PIPE-BR PIPE-TR PIPE-CROSS PIPE-TL PIPE-TR PIPE-TL PIPE-TL PIPE-BR PIPE-CROSS PIPE-BR PIPE-CROSS PIPE-TB PIPE-BR)))
(define GS_INIT (gamestate-init 5 2 2 DOWN (list PIPE-TL PIPE-TR PIPE-BL)))
(define GS_EXAMPLE2 (gamestate-init 7 2 3 LEFT (list PIPE-TL PIPE-TR PIPE-BL PIPE-CROSS PIPE-BR)))

(define example-gamestate
  (make-gamestate
   (make-grid 6 (list (make-coord-of-pipe (make-pipe #true #false #true #false) 2 4)
                      (make-coord-of-pipe (make-pipe #false #false #true #true) 2 3)
                      (make-coord-of-pipe (make-pipe #false #true #false #true) 2 1)
                      (make-coord-of-pipe (make-pipe #true #true #true #true) 3 1)
                      (make-coord-of-pipe (make-pipe #true #false #false #true) 4 1)
                      (make-coord-of-pipe (make-pipe #true #false #true #false) 4 2)
                      (make-coord-of-pipe (make-pipe #true #true #false #false) 3 2)
                      (make-coord-of-pipe (make-pipe #true #true #true #true) 2 2)
                      (make-coord-of-pipe (make-pipe #false #true #true #false) 1 2)
                      (make-coord-of-pipe (make-pipe #false #false #false #true) 1 1)))
   (list (make-pipe #false #true #false #true) (make-pipe #true #true #false #false) (make-pipe #false #false #true #true) (make-pipe #true #true #true #true)
    (make-pipe #true #false #true #false)) 60 20
 (make-coord-of-pipe (make-pipe #false #false #false #true) 1 1)
 (make-gooflow
  (list "RIGHT")
  (list
   (make-coord-of-pipe
    (make-pipe #false #false #false #true) 1 1))) 0 0))


