(in-package #:gol)

(defun cell () 1)

(defun new-cells ()
  (make-hash-table :test 'equal))

(defun cell-at (x y cells)
  (gethash (list x y) cells 0))

(defun set-cell-at (cells x y status)
  (if (eq status 'alive)
      (setf (gethash (list x y) cells) 1)
      (remhash (list x y) cells)
      ))

(defun alivep (cell)
  (= cell 1))

(defparameter neighbor-offsets
  '((-1 -1) (0 -1) (1 -1)
    (-1  0)        (1  0)
    (-1  1) (0  1) (1  1)
    ))

(defun neighbors (x y cells)
  (let ((coll '()))
    (dolist (offset neighbor-offsets)
      (if (alivep (cell-at (+ x (car offset))
			   (+ y (cadr offset))
			   cells))
	  (setf coll (cons 1 coll))
	  (setf coll (cons 0 coll)))
      )
    (reverse coll)
    ))

(defun neighbor-locations (x y)
  (let ((coll '()))
    (dolist (offset neighbor-offsets)
      (setf coll (cons (list (+ x (car offset))
			     (+ y (cadr offset)))
		       coll))
      )
    (reverse coll)
    ))

(defun living-neighbor-count (neighbors)
  (reduce '+ neighbors))

(defun living-neighbors-count-at (x y cells)
  (living-neighbor-count (neighbors x y cells)))


(defun next-status-at (x y cells)
  (let ((cell (cell-at x y cells))
	(neighbors (living-neighbors-count-at x y cells)))
    (cond
      ((and (alivep cell)
	    (< neighbors 2))
       'dead)
      ((and (alivep cell)
	    (or (= neighbors 2)
		(= neighbors 3)))
       'alive)
      ((and (alivep cell)
	    (> neighbors 3))
       'dead)
      ((and (not (alivep cell))
	    (= neighbors 3))
       'alive)
      (t 'dead) 
     )))


(defun make-set ()
  (make-hash-table :test 'equal))

(defun add (key set)
  (setf (gethash key set) 1))

(defun items (set)
  (alexandria:hash-table-keys set))


(defun next-generation (cells &optional (gens 1))
  (dotimes (n gens)
    (let ((next-gen (get-next-generation cells)))
      (setf cells next-gen))
    )
  cells)


(defun get-next-generation (cells)
  (let ((potentials (make-set))
	(next-gen (new-cells)))
    (labels ((update (pos status)
	       (dolist (p (cons pos
				(neighbor-locations (car pos)
						    (cadr pos))))
		 (add p potentials)
		     )))
      (maphash #'update cells))
    ;; (print potentials)
    (dolist (pos (items potentials))
      (let ((x (car pos))
	    (y (cadr pos)))
	(when (equal (next-status-at x y cells)
		     'alive)
	  (set-cell-at next-gen x y 'alive)))
      )
    next-gen
    )
  )

(defun cells-in-region (x y width height cells)
  ;(format t "(~a,~a) width:~a height:~a ~%" x y width height)
  ;(format t "~a" (alexandria:hash-table-keys cells))
  (let ((coll '()))
    (dotimes (x-offset width)
      (dotimes (y-offset height)
	(let ((tx (+ x x-offset))
	      (ty (+ y y-offset)))
	  (when (alivep (cell-at tx ty cells))
	    (setf coll (cons (list tx ty) coll))))))
    coll))
