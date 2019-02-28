;;;; gol.asd

(asdf:defsystem #:gol
  :description "Describe Game of Life here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:sdl2)
  :components ((:file "package")
	       (:file "frame")
	       (:file "camera")
	       (:file "draw")
	       (:file "grid")
	       (:file "life")
	       (:file "gol")))
