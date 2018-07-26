(in-package :cards-sim)

(defvar *canvas-width* 800)
(defvar *canvas-height* 600)

(defconstant +card-width+ 60)
(defconstant +card-height+ 100)

(defvar *black* (gamekit:vec4 0 0 0 1))
(defvar *light-grey* (vec4 0.8 0.8 0.8 1))
(defvar *yellow* (vec4 1 1 0 1))
(defvar *green* (vec4 0 1 0 1))

(defvar *mouse-pos* (gamekit:vec2 0 0))
(defvar *mouse-x* 0)
(defvar *mouse-y* 0)

(defvar *reactions* nil)
(defvar *card-data-db* (make-hash-table :test #'eql))

(defvar *cards* (make-list 0))
(defvar *grabbed-card* nil)
(defvar *hovered-card* nil)

(defvar *grab-offset-x* 0)
(defvar *grab-offset-y* 0)
(defvar *grab-offset* (vec2 0 0))

;; constants for panel
(defconstant +panel-section-width+  16)
(defconstant +panel-section-height+ 16)

(defconstant +panel-topleft-pos+     (vec2 0 0))
(defconstant +panel-topright-pos+    (vec2 48 0))
(defconstant +panel-bottomleft-pos+  (vec2 0 48))
(defconstant +panel-bottomright-pos+ (vec2 48 48))

(defconstant +panel-top-pos+ (vec2 16 0))
(defconstant +panel-left-pos+ (vec2 0 16))
(defconstant +panel-bottom-pos+ (vec2 16 48))
(defconstant +panel-right-pos+ (vec2 48 16))
