;;;; triangulator.asd

(asdf:defsystem #:triangulator
  :description "Describe triangulator here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:sdl2 #:swank #:trivia #:alexandria #:jonathan)
  :components ((:file "package")
               (:file "triangulator")))
