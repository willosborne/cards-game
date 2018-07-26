(in-package :cards-sim)

(defclass slot (renderable positionable)
  ((description
    :initarg :description
    :reader description)
   )
  )
