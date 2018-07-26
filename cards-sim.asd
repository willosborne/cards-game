;;;; cards-sim.asd

(asdf:defsystem #:cards-sim
  :description "Describe cards-sim here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:trivial-gamekit)
  :components ((:file "package")
               (:file "globals")
               (:file "resources")
               (:file "utils")
               (:file "card-manager")
               (:file "cards-sim")
               (:file "card")))
