(in-package #:gol)

(defclass grid (frame)
  ((rows :initform 1600 :initarg :rows)
   (columns :initform 800 :initarg :columns)
   (cell-width :initform 10 :initarg :cell-width :accessor cell-width)
   (grid-cells :initform nil :accessor grid-cells)
   ))

(defmethod print-object ((g grid) stream)
  (format stream "<Grid ~a x ~a>" (height g) (width g)))

(defclass grid-cell ()
  ((color :initform nil :initarg :color :accessor color)
   (rect :initform nil :initarg :rect :accessor rect)
   (value :initform nil :initarg :value :accessor value)))

(defmethod print-object ((cell grid-cell) stream)
  (format stream "#<Cell ~a ~a>" (color cell) (rect cell))
  )

(defparameter *empty-cell-value* 'empty)

(defmethod empty-cellp ((cell grid-cell))
  (eq (value cell) *empty-cell-value*))

(defmethod clear-cell ((cell grid-cell))
  (setf (value cell) *empty-cell-value*))

(defun make-grid (graphics)
  (let ((g (make-instance 'grid :graphics graphics)))
    (reset-cells g)
    g))

(defmethod reset-cells ((g grid))
  (with-slots (rows columns grid-cells) g
    ;; (when (not (null grid-cells))
    ;;   (mapgrid g
    ;; 	       (lambda (c)		 
    ;; 		 (sdl2:free-rect (rect c))))
    ;;   )
    
    (setf grid-cells (make-array
		      (list rows columns)
		      :initial-element nil))
    (mapgrid g
	     (lambda (row column)
	       (insert-cell g row column))
	     :coords-only t)))



(defmethod insert-cell ((g grid) row column)
  (let ((cell (make-instance 'grid-cell))
	(width (cell-width g)))
    (clear-cell cell)
    (with-slots (color rect) cell
      (setf (color cell) '(255 0 0))
      (setf (rect cell) (make-rectangle g
					(* column width)
					(* row width)
					width
					width
					)))
    (setf (grid-cell-at g row column) cell)))

(defmethod clear-cells ((g grid))
  (mapgrid g #'clear-cell)
  )

(defmethod mapgrid ((g grid) func &key
				    ((:start-row start-row) 0)
				    ((:start-column start-column) 0)
				    ((:stop-row stop-row) nil)
				    ((:stop-column stop-column) nil)
				    ((:enumerate enumerate) nil)
				    ((:coords-only coords-only) nil)
				    )
  (with-slots (rows columns) g
    (dotimes (row (- (if stop-row stop-row rows) start-row))
      (dotimes (column (- (if stop-column stop-column columns) start-column))
	(let ((r (+ start-row row))
	      (c (+ start-column column)))
	  (cond
	    (coords-only (funcall func r c))
	    (enumerate (funcall func (grid-cell-at g r c) r c))
	    (t (funcall func (grid-cell-at g r c)))
	    ))))))



(defmethod width ((g grid))
  (with-slots (columns cell-width) g
    (* columns cell-width)))

(defmethod height ((g grid))
  (with-slots (rows cell-width) g
    (* rows cell-width)))

(defmethod grid-cell-at ((g grid) row column)  
  (aref (grid-cells g) row column))

(defmethod (setf grid-cell-at) (new-cell (g grid) row column)
  (setf (aref (grid-cells g) row column) new-cell))

(defmethod value-at ((g grid) row column)
  (value (grid-cell-at g row column)))

(defmethod (setf value-at) (value (g grid) row column)
  (setf (value (grid-cell-at g row column)) value))

(defmethod color-at ((g grid) row column)
  (color (grid-cell-at g row column)))

(defmethod (setf color-at) (new-color (g grid) row column)
  (setf (color (grid-cell-at g row column)) new-color))

(defmethod rect-at ((g grid) row column)
  (rect (grid-cell-at g row column)))

(defmethod (setf rect-at) (new-rect (g grid) row column)
  (setf (rect (grid-cell-at g row column)) new-rect))

(defmethod draw ((g grid))
  ;;(draw-lines g)
  (mapgrid g (lambda (cell)
  	       ;;(format t "~a~%" cell)
  	       (draw-cell g cell)
  	       ))
  ;;(draw-cell g (grid-cell-at g 0 0))
  ;;(print (grid-cell-at g 10 0))
  )

(defmethod draw-cell ((g grid) (cell grid-cell))
  (when (not (empty-cellp cell))
    (destructuring-bind (red green blue) (color cell)
      (set-color g red green blue)
      (draw-saved-fill-rectangle g (rect cell))
      )))




(defmethod draw-lines ((g grid))
  (with-slots (rows columns cell-width) g 
    (let* ((x 0)
	   (y 0)
	   (width cell-width)
	   (grid-width (* width columns))
	   (grid-height (* width rows)))
      (dotimes (r (+ 1 rows))
	(draw-line g x y grid-width y :absolute t)
	(setf y (+ y width))
	)
      (setf y 0)
      (dotimes (c (+ 1 columns))
	(draw-line g x y x grid-height :absolute t)
	(setf x (+ x width))))))

(defmethod fill-cell ((g grid) row column)
  (with-slots (rows columns cell-width) g
    (destructuring-bind (cell-x cell-y) (row-column-to-coords g row column)	
      (draw-filled-rectangle g (+ 1 cell-x) (+ 1 cell-y)
			(- cell-width 1) (- cell-width 1)))
    ))


(defmethod set-grid-zoom ((g grid) zoom-offset)
  ;;(clear grid)
  (with-slots (cell-width rows columns) g
    (let ((w (+ cell-width zoom-offset)))
      (when (> w 0)
	(setf cell-width w)
	(setf rows (ceiling (/ *window-height* w)))
	(setf columns (ceiling (/ *window-width* w)))
	)
      )
    (reset-cells g)
    )
  )


(defmethod toggle-cell-status ((g grid) row column)
  (format t "Status toggle~%")
  ;;(fill-cell g row column)
  (let ((cell (grid-cell-at g row column)))
    (setf (color cell) '(255 0 0))
    (if (empty-cellp cell)
	(setf (value cell) 'not-empty)
	(clear-cell cell))
    )
  ;;(draw-cell g (grid-cell-at g row column))
  )


;; (defmethod highlight-cell ((g grid) row column)
;;   (with-slots (rows columns cell-width) g
;;     (destructuring-bind (cell-x cell-y)(row-column-to-coords g row column)
;;       (draw-rectangle g cell-x cell-y
;; 		 (+ 1 cell-width) cell-width))
;;     ))



(defmethod set-pos ((g grid) x y)
  (destructuring-bind (row column) (coords-to-row-column g  x y)
    (destructuring-bind (snap-x snap-y) (row-column-to-coords g row column)
      (setf (x g) snap-x)
      (setf (y g) snap-y)
      )))

(defmethod get-pos ((g grid) x y)
  (let ((x (+ (x g) x))
	(y (+ (y g) y)))
    (coords-to-row-column g x y))
  )

(defmethod get-length ((g grid) n)
  (floor (/ n (cell-width g))))

(defmethod in-bounds ((g grid) row column)
  (with-slots (rows columns) g
    (and (>= row 0) (< row rows)
	 (>= column 0) (< column columns))
    ))

(defmethod row-column-to-coords ((g grid) row column)
  (with-slots (cell-width row-offset column-offset) g 
    (list (* cell-width column) (* cell-width row)))
  )

(defmethod coords-to-row-column ((g grid) x y)
  (with-slots (cell-width row-offset column-offset) g
    (list (floor (/ y cell-width)) (floor (/ x cell-width))
	  )))




