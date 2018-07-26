(in-package :cards-sim)

(define-font :swagger "Swagger.ttf")

(defvar *card-description-font* nil)

(define-image :test-image "snake-head.png")
(define-image :panel-template "panel.png")

(define-image :bloke-icon "icons/004-becquer.png")
(define-image :manuscript-icon "icons/020-manuscript.png")

(defun make-fonts ()
  (setf *card-description-font* (make-font :swagger 24)))
