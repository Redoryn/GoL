;;;; GoL.lisp
;;;; Helper draw functions
(in-package #:gol)

(defclass graphics ()
  ((renderer :initform nil :initarg :renderer :accessor renderer)
   (window :initform nil :initarg :window :accessor window)))

(defun make-graphics (renderer window)
  (let ((g (make-instance 'graphics)))
    (with-slots ((g-renderer renderer) (g-window window)) g
      (setf g-renderer renderer)
      (setf g-window window)
      )
    g))

(defmethod make-rectangle ((g graphics) x y w h)
  (sdl2:make-rect x y w h))

(defmethod render-rect ((g graphics) rect)
  (sdl2:render-draw-rect (renderer g) rect))

(defmethod draw-line ((g graphics)x1 y1 x2 y2 &key ((:absolute absolute) nil))
  (sdl2:render-draw-line (renderer g) x1 y1 x2 y2))

(defmethod draw-rect ((g graphics) x y w h &key ((:absolute absolute) nil))
  (sdl2:render-draw-rect (renderer g) (sdl2:make-rect x y w h)))



(defmethod draw-filled-rect ((g graphics) x y w h)
  (sdl2:render-fill-rect (renderer g) (sdl2:make-rect x y w h)))

(defmethod render-fill-rect ((g graphics) rect)
  (sdl2:render-fill-rect (renderer g) rect))


(defmethod clear ((g graphics))
  (sdl2:set-render-draw-color (renderer g) 0 0 0 255)
  (sdl2:render-clear (renderer g)))

(defmethod set-color ((g graphics) red green blue &optional (a 255))
  (sdl2:set-render-draw-color (renderer g) red green blue a))

(defmethod draw-to-screen ((g graphics))
  (sdl2:render-present (renderer g)))
