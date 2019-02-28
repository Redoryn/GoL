(in-package #:gol)

(defun begin-mouse-selection (x y state)
  ())

(defun on-mouse-down (button state)
  (if (= button 1)
      (begin-mouse-selection x y state))
  state)

(defun on-mouse-up (button state)
  (if (= button 1)
      (end-mouse-selection x y state))
  state)
