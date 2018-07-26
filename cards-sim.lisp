;;;; cards-sim.lisp

(in-package #:cards-sim)

(gamekit:register-resource-package :keyword
                                   (asdf:system-relative-pathname :cards-sim "assets/"))

(defgame cards-sim () ()
                 (:viewport-width *canvas-width*)
                 (:viewport-height *canvas-height*)
                 (:viewport-title "Cards simulator"))


(defun mouse-down (x y)
  (format t "Mouse down, x: ~d y: ~d" x y)
  (setf *grabbed-card* (top-card-at-point x y))
  (when *grabbed-card*
    ;; Move grabbed card to top of pile
    (setf *cards* (cons *grabbed-card* (remove *grabbed-card* *cards*)))
    (let ((x-off (- (x (position-of *grabbed-card*)) x))
          (y-off (- (y (position-of *grabbed-card*)) y))
          (offset (subt (position-of *grabbed-card*) *mouse-pos*)))
      (setf *grab-offset-x* x-off
            *grab-offset-y* y-off
            *grab-offset* offset))))

(defun mouse-up (x y)
  (declare (ignorable x y))
  (when (and *grabbed-card*
             *hovered-card*
             (cards-compatible-p *grabbed-card* *hovered-card*))
    (react-cards *grabbed-card* *hovered-card*))
  (setf *grabbed-card* nil))

(defun mouse-move (x y)
  (declare (ignorable x y))
  (let* ((cards (cards-at-point x y))
         (hovered (second cards)))
    (if (and hovered 
             *grabbed-card*
             (cards-compatible-p *grabbed-card* hovered))
        ;; green glow on secondary card
        (setf *hovered-card* hovered)
        (setf *hovered-card* nil))))

;; (defun key-down (key))

(defmethod gamekit:post-initialize ((app cards-sim))
  (bind-cursor (lambda (x y)
                 (setf (gamekit:x *mouse-pos*) x
                       (gamekit:y *mouse-pos*) y
                       *mouse-x* x
                       *mouse-y* y)
                 (mouse-move x y)))
  (bind-button :mouse-left :pressed
               (lambda ()
                 (mouse-down *mouse-x* *mouse-y*)))
  (bind-button :mouse-left :released
               (lambda ()
                 (mouse-up *mouse-x* *mouse-y*)))
  (bind-button :escape :released
               (lambda ()
                 (gamekit:stop)))
  (bind-button :mouse-right :released
               (lambda ()
                 (kill-grabbed-card)))
  (make-fonts))

(defun kill-grabbed-card ()
  (when *grabbed-card*
    (setf *cards* (remove *grabbed-card* *cards*))
    (setf *grabbed-card* nil)))

(defun top-card-at-point (x y)
  "Get and return the first card at the given point, or nil if no card is at that point"
  (car (cards-at-point x y)))

(defun cards-at-point (x y)
  (remove-if-not #'(lambda (card) (point-in-card-p (vec2 x y) card)) *cards*))

(defun real-time-seconds ()
  (/ (get-internal-real-time) internal-time-units-per-second))

(defmethod gamekit:act ((app cards-sim))
  (when *grabbed-card*
    (let ((target (add *mouse-pos* *grab-offset*)))
      (setf (position-of *grabbed-card*) (lerp (position-of *grabbed-card*) target 0.4)))))

;; (defvar *panel* (make-instance 'panel :render-image :panel-template
;;                                :width 256
;;                                :height 256
;;                                :position (vec2 100 0)))

(defmethod gamekit:draw ((app cards-sim))
  (when *grabbed-card*
    (loop for card in (compatible-cards *grabbed-card* *cards*) do
         (render-glow card)))
  (when *hovered-card*
    (render-glow *hovered-card* :color *green*))
  
  (loop for card in (reverse *cards*) do
       (render card))
  
  (draw-text "Drag 'n' drop; delete while dragging with RMB" (vec2 10 10))
  ;; (draw-section (vec2 200 200) (vec2 0 0) 32 32 :panel-template)
  ;; (draw-image (vec2 100 100) :panel-template
  ;;             :origin (vec2 16 16)
  ;;             :width 100
  ;;             :height 100)
  ;; (render *panel*)
  )

(defun run ()
  (unless (gamekit:gamekit)
    (start 'cards-sim)))


