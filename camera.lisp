(in-package #:gol)


(defclass camera (frame)
  ((zoom-depth   :initform 1 :initarg :zoom-depth :accessor zoom-depth)
   ))

(defun make-camera (graphics)
  (let ((camera (make-instance 'camera)))
    (setf (graphics camera) graphics)
    camera))
