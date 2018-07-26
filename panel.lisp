(in-package :cards-sim)

(defclass panel (positionable renderable)
  ((width
    :initarg :width
    :initform (error "Must supply width")
    :reader width)
   (height
    :initarg :height
    :initform (error "Must supply height")
    :reader height)
   (texture-coords
    :initarg :texture-coords
    :initform "Must supply texture coords!"
    :reader texture-coords)
   (section-width
    :initarg :section-width
    :initform 16
    :reader section-width)
   (section-height
    :initform 16
    :reader section-height)
   (image
    :initarg :render-image
    :initform (error "Must supply render texture!")
    :accessor render-image)))

(defun (setf width) (value panel)
  (setf (slot-value panel 'width) 
        (* (section-width panel) (max 1 (floor (/ value (section-width panel)))))))

(defun (setf height) (value panel)
  (setf (slot-value panel 'height) 
        (* (section-height panel) (max 1 (floor (/ value (section-height panel)))))))

(defun (setf x) (value panel)
  (setf (x (position-of panel)) value))

(defun (setf y) (value panel)
  (setf (y (position-of panel)) value))

(defgeneric (setf width) (value panel))
(defgeneric (setf height) (value panel))

(defmethod (setf width) (panel panel))

(defmethod render ((panel panel))
  (with-slots (width height section-width section-height image) panel
    (let* ((bottom-left (position-of panel))
           (bottom-right (add bottom-left (vec2 (- width section-width) 0)))
           (top-left (add bottom-left (vec2 0 (- height section-height))))
           (top-right (add bottom-left (vec2 (- width section-width) (- height section-height))))
           (width-tiles (/ width section-width))
           (height-tiles (/ height section-height)))
      (draw-image bottom-left image
                  :origin (vec2 0 0)
                  :width section-width
                  :height section-height)
      (draw-section bottom-left  (vec2 0 0) section-width section-height image)
      (draw-section bottom-right (vec2 3 0) section-width section-height image)
      (draw-section top-left     (vec2 0 3) section-width section-height image)
      (draw-section top-right    (vec2 3 3) section-width section-height image)
      (loop for x from 1 to (- width-tiles 2) do
           (draw-section (add bottom-left (vec2 (* x section-width) 0)) (vec2 1 0) section-width section-height image)
           (draw-section (add top-left (vec2 (* x section-width) 0)) (vec2 1 3) section-width section-height image))
      (loop for y from 1 to (- height-tiles 2) do
           (draw-section (add bottom-left (vec2 0 (* y section-height))) (vec2 0 1) section-width section-height image)
           (draw-section (add bottom-right (vec2 0 (* y section-height))) (vec2 3 1) section-width section-height image))
      (loop for x from 1 to (- width-tiles 2) do
           (loop for y from 1 to (- height-tiles 2) do
                (draw-section (add bottom-left (vec2 (* x section-width) (* y section-height)))
                              (vec2 1 1) 
                              section-width
                              section-height 
                              image))))))

(defun draw-section (position coords width height image)
  (draw-image position image
              :origin (mult coords (vec2 width (+ height 1)))
              :width width
              :height height))
