(in-package #:gol)

(defclass frame ()
  ((graphics :initform nil :initarg :graphics :accessor graphics)
   (x :initform 0 :initarg :x :accessor x)
   (y :initform 0 :initarg :y :accessor y)))


(defmethod make-rectangle ((f frame) x y width height)
  (make-rectangle (graphics f) x y width height))


(defmethod draw-rectangle ((f frame) x y width height &key ((:absolute absolute) nil))
  (if absolute
      (draw-rect (graphics f) x y width height)
      (draw-rect (graphics f) (+ (x f) x) (+ (y f) y) width height)
      ))

(defmethod draw-saved-rectangle ((f frame) rect)
  (render-rect (graphics f) rect))

(defmethod draw-saved-fill-rectangle ((f frame) rect)
  (render-fill-rect (graphics f) rect))

(defmethod draw-line ((f frame) x1 y1 x2 y2 &key ((:absolute absolute) nil))
  (if absolute
      (draw-line (graphics f) x1 y1 x2 y2)
      (draw-line (graphics f)
		 (+ (x f) x1) (+ (y f) y1)
		 (+ (x f) x2) (+ (y f) y2))))

(defmethod draw-filled-rectangle ((f frame) x y width height)
  (draw-filled-rect (graphics f) (+ (x f) x) (+ (y f) y) width height))

(defmethod clear ((f frame))
  (clear (graphics f)))

(defmethod set-color ((f frame) r g b &optional (a 255))
  (set-color (graphics f) r g b a))

(defmethod flush-to-screen ((f frame))
  (draw-to-screen (graphics f)))

(defmethod pos-in-frame ((f frame) x y)
  (list (+ (x f) x) (+ (y f) y)))
