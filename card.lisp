(in-package :cards-sim)

(defclass positionable ()
  ((position 
    :initarg :position 
    :initform (vec2 0 0) 
    :reader position-of)
   (angle 
    :initarg :angle 
    :initform 0 
    :reader angle-of)))

(defun set-position (card x y)
  (setf (x (position-of card)) x
        (y (position-of card)) y))

(defgeneric (setf position-of) (vec2 positionable))

(defmethod (setf position-of) ((vec vec2) (this positionable))
  (setf (x (position-of this)) (x vec)
        (y (position-of this)) (y vec)))

(defmethod (setf angle-of) (value (this positionable))
  (setf (slot-value this 'angle) value))

(defgeneric render (object)
  (:method (object) (declare (ignore object))))

(defclass renderable () ())

(defmethod render :around ((this renderable))
  (with-pushed-canvas ()
    (call-next-method)))

(defclass card (positionable renderable)
  ((type 
    :initarg :type
    :reader type)
   (image
    :initarg :image
    :accessor image)
   (name
    :initarg :name
    :reader name)
   (description
    :initarg :description
    :reader description)
   (defined-p
    :initarg :defined-p
    :reader defined-p)))

(defmethod print-object ((object card) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "Type: :~a" (type object))))

(defmethod render ((card renderable))
  (with-slots (position image) card
    (gamekit:draw-rect position +card-width+ +card-height+ :fill-paint *light-grey* :stroke-paint *black*)
    (with-pushed-canvas ()
      (translate-canvas (x position) (y position))
      (scale-canvas 0.1 0.1)
      (draw-image (vec2 0 0) image))
    (render-name card)))

(defun render-name (card)
  (let ((position (position-of card))
        (name (if (defined-p card)
                  (name card)
                  (string (type card)))))
    (draw-text name (add position (vec2 -10 -20)) :fill-color *black* :font *card-description-font*)))

(defun render-glow (card &key (color *yellow*))
  "Render the 'glow' around a suggested card"
  (with-slots (position) card
    (draw-rect (subt position (vec2 5 5)) (+ 10 +card-width+) (+ 10 +card-height+) :fill-paint color)))

(defun point-in-card-p (point card)
  "Test if a point is contained within the boundaries of a card"
  (let ((card-x (x (position-of card)))
        (card-y (y (position-of card)))
        (point-x (x point))
        (point-y (y point)))
    (not (or (> point-x (+ card-x +card-width+))
             (< point-x card-x)
             (> point-y (+ card-y +card-height+))
             (< point-y card-y)))))
