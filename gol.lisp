;;;; GoL.lisp

(in-package #:gol)



(defun initialize-gol ()
  (let ((cells (new-cells)))
    (set-cell-region 100 100 27 27 cells)
    cells
    ))

(defun set-cell-region (x y w h cells)
  (dotimes (x-offset w)
    (dotimes (y-offset h)
      (set-cell-at cells (+ x x-offset) (+ y y-offset) 'alive)
      ))
  cells)


(defun highlight-cells-in-region (grid camera cells window-region)
  (destructuring-bind (x y width height) window-region
    (set-color grid 255 255 0)
    (when left-click-held
      (draw-rectangle grid x y width height :absolute t))
    (destructuring-bind (row column) (coords-to-row-column grid x y)
      (destructuring-bind (end-row end-column) (coords-to-row-column
						grid
						(+ x width)
						(+ y height))	
	;;(format t "~a,~a to ~a,~a~%" row column end-row end-column)
	(mapgrid grid
		 (lambda (c gr gc)
		   (when (not (empty-cellp c))
		     (setf (color c) '(0 255 0))
		     ;;(highlight-cell grid gr gc)
		     )
		   )
		 :enumerate t
		 :start-row row
		 :stop-row end-row
		 :start-column column
		 :stop-column end-column
		 )
	))))




(defun in-camera-view (camera x y)
  (not (or (< x (x camera))
	   (< y (y camera))
	   (> x (+ (x camera) (width camera)))
	   (> y (+ (y camera) (height camera))))
       )
  )

(defmethod grid-offsets ((g grid) (c camera))
  (coords-to-row-column g (x c) (y c)))


(defmethod draw-camera ((c camera))
  (set-color c 0 255 0)
  (draw-rectangle c 0 0 (width c) (height c) :absolute t))


(defmethod draw-gol ((g grid) (c camera)  cells selected-region)
  (clear g)
  (set-color g 100 0 0)
  (draw g)
  (set-color g 255 0 0)  
  (when selected-region
    (highlight-cells-in-region g
			       c
  			       cells
  			       selected-region)
    )
  )


(defclass app ()
  ((grid :initform nil :initarg :grid :accessor display-grid)
   (cells :initform nil :initarg :cells :accessor cells)
   (highlight-region :initform nil :accessor highlight-region)
   (camera :initform nil :initarg nil :accessor camera)
   (debug-info :initform nil :accessor debug-info)
   ))

(defmethod grid ((a app))
  (display-grid a))

(defun make-app (graphics)
  (let ((app (make-instance 'app)))
    (with-slots (grid cells camera) app
      (setf grid (default-grid graphics))
      (setf cells (initialize-gol))
      (setf camera (make-camera graphics)))
    app))

(defun default-grid (graphics)
  (let ((grid (make-grid graphics)))
    (with-slots (rows columns cell-width width height) grid
      (setf rows 400)
      (setf columns 800)
      (setf cell-width 2)
      )
    grid))


(defparameter *board-command* 'play)
(defparameter *params* nil)


(defun create (graphics)
  (make-app graphics))
 
(defun reset ()
  (setf *board-command* 'reset))

(defun clear-board ()
  (setf *board-command* 'clear-board))

(defun pause ()
  (setf *board-command* 'pause))

(defun play ()
  (setf *board-command* 'play))

(defun copy (cells x y)
  (setf *board-command* 'copy)
  (setf *params* (list cells x y))
  )


(defun reset-to (w h)
  (setf *board-command* 'reset-to)
  (setf *params* (list w h)))

(defun update-grid-cells (grid cells))

(defun top-left-pos () '(0 0))

;; (defun bind-cells-to-grid (grid camera cells)
;;   (destructuring-bind (row-offset column-offset) (grid-position grid camera 0 0)    
;;     (mapgrid grid
;; 	     (lambda (c row column)
;; 	       (if (alivep
;; 		    (cell-at (+ column-offset column)
;; 			     (+ row-offset row)
;; 			     cells))
;; 		   (setf (value c) (list (+ column-offset column)
;; 					 (+ row-offset row)))
;; 		   (clear-cell c)))
;; 	     :enumerate t)))


(defun bind-cells-to-grid (grid camera cells)
  (clear-cells grid)
  (maphash (lambda (coords value)
	     (destructuring-bind (row column)
		 (grid-row-column-from-cell-coords grid
						   camera
						   (car coords)
						   (cadr coords))
	       (when (in-bounds grid row column)
		 (setf (value (grid-cell-at grid row column)) 'not-empty)
		 )))
	   cells))


(defun update-state (state)
  (let ((grid (grid state))
	(cells (cells state))
	(region (highlight-region state))
	(camera (camera state)))
    (if (not (eq *board-command* 'play))
	(progn
	  (case *board-command*
	    ('reset (setf *board-command* 'play)
		    (setf state (create (graphics camera))))
	    ('clear-board (setf *board-command* 'play)
			  (setf (cells state) (new-cells)))
	    ('pause state)
	    ('reset-to (setf *board-command* 'play)
		       (setf (cells state)
			     (set-cell-region 100
					      100
					      (car *params*)
					      (cadr *params*)
					      (new-cells))
			     ))
	    ('copy (setf *board-command* 'play)
		   (setf (cells state)
			 (copy-to cells
				  (car *params*)
				  (cadr *params*)
				  (caddr *params*))))
	    )
	  state
	  )
	(progn
	  (setf (cells state) (next-generation cells))
	  (bind-cells-to-grid grid camera cells)
	  (when (not region)
	    ;;(reset-grid-cell-color grid)
	    )
	  state
	  ))))

(defmethod reset-grid-cell-color ((g grid))
  (mapgrid g (lambda (c) (setf (color c) '(255 0 0)))))

(defun draw-state (state)
  (draw-gol (grid state)
	    (camera state)
	    (cells state)
	    (highlight-region state))
  (flush-to-screen (grid state)))

(defun selection-region (x y)
  (let ((x (min x mousedown-x))
	(y (min y mousedown-y))
	(width (abs (- mousedown-x x)))
	(height (abs (- mousedown-y y))))
    (list x y width height)
  ))

(defun vector-magn (x y)
  (sqrt (+ (* x x) (* y y))))

(defun vector-mul (x y n)
  (list (* x n) (* y n)))

(defun vector-normalize (x y)
  (let ((m (vector-magn x y)))
    (if (= m 0)      
	(list 0 0)
	(list (/ x m) (/ y m))
      ))) 

(defun move-camera (x y xrel yrel state)
  (let ((c (camera state)))
    (setf (x c) (+ (x c) xrel))
    (setf (y c) (+ (y c) yrel))
    c))


(defun on-mousemotion (state x y xrel yrel button)
  ;;(format t "(~a,~a)~%" (x (grid state)) (y (grid state)))
  (let ((grid (grid state))
	(cells (cells state)))
    (cond
      (left-click-held
       (let ((region (selection-region x y)))
	 (setf (highlight-region state) region)))
      (right-click-held
       (let ((c (move-camera x y xrel yrel state)))
	 (setf (camera state) c))
       )
      (t state))
    state))

 
(defmethod grid-position ((g grid) (c camera) x y)
  (let* ((offsets (grid-offsets g c))
	 (offset-row (car offsets))
	 (offset-column (cadr offsets)))
    (destructuring-bind (row column) (coords-to-row-column g x y)
      (list (- row offset-row) (- column offset-column)))))

(defmethod gol-cell-position ((g grid) (c camera) x y)
  (destructuring-bind (offset-row offset-column) (grid-position g c 0 0)
    (destructuring-bind (row column) (coords-to-row-column g x y)
      (list (+ column offset-column) (+ row offset-row))
      )
    ))

(defmethod grid-row-column-from-cell-coords ((g grid) (c camera) x y)
  (destructuring-bind (row column) (coords-to-row-column g (x c) (y c))
    (list (+ row y) (+ column x))
    )
  )

(defun on-cell-click (grid camera x y)
  (destructuring-bind (row column) (coords-to-row-column grid x y)
    ;;(toggle-cell-status grid row column)
    (format t "Window Coords: ~a,~a~%" x y)
    (format t "Grid: ~a,~a Life Coords: ~a~%" row column  (value (grid-cell-at grid row column)))
    (let ((cell-pos (value (grid-cell-at grid row column))))
      (when (listp cell-pos)
	(let ((pos (grid-row-column-from-cell-coords grid
						      camera
						      (car cell-pos)
						      (cadr cell-pos))))
	  (format t "Computed Grid Coords: ~a~%" pos))))
    ))

  ;; (destructuring-bind (gx gy) (grid-position grid camera x y)
  ;;   (format t "Clicked: (~a,~a) -> (~a,~a)~%" x y gx gy)
  ;;   (toggle-cell-status grid gx gy)
  ;;   ))

(defun identify-pos-of-camera (camera)
  (format t "Camera: (~a,~a)~%" (x camera) (y camera)))


(defparameter mousedown-x 0)
(defparameter mousedown-y 0)
(defparameter left-click-held nil)
(defparameter right-click-held nil)

(defun on-mousedown (button x y state)
  (format t "Button: ~a~%" button)
  (when (= button 1)
    (setf left-click-held t)
    (identify-pos-of-camera (camera state))
    (on-cell-click (grid state) (camera state) x y)
    )
  (when (= button 3)
    (setf right-click-held t))
  (setf mousedown-x x)
  (setf mousedown-y y)
  (setf (highlight-region state) nil)
  state
  )

(defun on-mouseup (button x y state)
  (setf left-click-held nil)
  (setf right-click-held nil)
  state)

(defun on-key (state keysym)
  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
    (sdl2:push-event :quit))
  state)

(defun on-mousewheel (direction x y state)
  (format t "Direction ~a~%"  y)
  (set-grid-zoom (grid state) y)
  (format t "Population: ~a" (hash-table-count (cells state)))
  state)


(defun app-main-loop (graphics)
  (let ((state (create graphics)))
    (sdl2:with-event-loop (:method :poll)
      (:keyup (:keysym keysym)
	      (setf state (on-key state keysym))
       )
      (:mousemotion (:x x :y y :xrel xrel :yrel yrel :state button)
		    (setf state (on-mousemotion state x y xrel yrel button)))
      (:mousebuttondown (:x x :y y :button button)
			(setf state
			      (on-mousedown button x y state)))
      (:mousebuttonup (:x x :y y :button button)
		      (setf state
			    (on-mouseup button x y state)))
      (:mousewheel (:x x :y y :direction direction)
		   (setf state
			 (on-mousewheel direction x y state)))
      (:idle ()	     
	     (draw-state state)
	     (setf state (update-state state))	     
	     (sdl2:delay *frame-rate*)
	     (force-output)
       )
      (:quit () t))
    )
  )


(defparameter *frame-rate* 33)
(defparameter *window-width* 1200)
(defparameter *window-height* 800)


(defun start ()
  "Test the SDL_render.h API"
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "Game of Life"
			   :w *window-width*
			   :h *window-height*
			   :flags '(:shown))
      (sdl2:with-renderer (renderer win :flags '(:accelerated))
	(app-main-loop (make-graphics renderer win))
	))))
