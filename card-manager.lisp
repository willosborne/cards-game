(in-package :cards-sim)

(defun add-card-data (card-type name description &key (image :manuscript-icon))
  (setf (gethash card-type *card-data-db*)
        (list :name name
              :description description
              :image image)))

(defun card-defined-p (card-type)
  (gethash card-type *card-data-db*))

(defun get-card-data (card-type data)
  (getf (gethash card-type *card-data-db*) data))

(defun delete-all-cards ()
  (setf *cards* (make-list 0)))

(defun destroy-card (card)
  (setf *cards* (remove card *cards*)))

(defun spawn-cards (card-types x y)
  "Spawn the given cards at the given position"
  (let ((dest-x x)
        (dest-y y))
    (loop for card-type in card-types do
         (spawn-card card-type dest-x dest-y)
         (setf dest-x (incf dest-x 10)
               dest-y (incf dest-y 10)))))

(defun spawn-card (card-type x y)
  (if (card-defined-p card-type)
      (let ((name (get-card-data card-type :name))
            (description (get-card-data card-type :description))
            (image (get-card-data card-type :image)))
        (push (make-instance 'card 
                             :position (vec2 x y) 
                             :type card-type 
                             :name name 
                             :description description 
                             :image image 
                             :defined-p t) *cards*))
      (push (make-instance 'card 
                           :position (vec2 x y) 
                           :type card-type 
                           :defined-p nil
                           :image :manuscript-icon) *cards*)))

(defclass reaction ()
  ((card-1
    :initarg :card-1
    :initform (error "Must supply first card")
    :reader card-1)
   (card-2
    :initarg :card-2
    :initform (error "Must supply second card")
    :reader card-2)
   ;; (required-cards
   ;;  :initarg :required-cards
   ;;  :initform (error "Must supply list of cards for required ingredients")
   ;;  :reader required-cards)
   (action
    :initarg :action
    :initform (error "Must supply action function")
    :reader action)))

(defmethod print-object ((object reaction) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "In: (:~a, :~a) ACTION: ~a" (card-1 object) (card-2 object) (action object))))

(defun do-nothing ())

(defun make-reaction (card-1 card-2 action)
  (push (make-instance 'reaction :card-1 card-1 :card-2 card-2 :action action) *reactions*))

(defun get-reaction (card-type-1 card-type-2)
  "Return the reaction for the given pair. 
NOTE: currently assuming reactions are symmetrical"
  (loop for reaction in *reactions* do
       (when (or (and (eql (card-1 reaction) card-type-1) (eql (card-2 reaction) card-type-2))
                 (and (eql (card-1 reaction) card-type-2) (eql (card-2 reaction) card-type-1)))
         (return reaction))))

(defun compatible-reactions (card-type)
  "Return the list of reactions with this card as an ingredient"
  ;; TODO consider whether reactions should be symmetric
  (remove-if-not #'(lambda (reaction)
                     (or (eql (card-1 reaction) card-type)
                         (eql (card-2 reaction) card-type)))
                 *reactions*))

(defun compatible-card-types (card-type)
  "Return the list of card types that have reactions with the given card type"
  (loop for reaction in (compatible-reactions card-type)
       collect (if (eql card-type (card-1 reaction))
                   (card-2 reaction)
                   (card-1 reaction))))

(defun card-types-compatible-p (card-type-1 card-type-2)
  "Return t if the two card types have a reaction"
  (member card-type-2 (compatible-card-types card-type-1) :test #'eql))

(defun compatible-cards (card cards)
  "Filter the given cards list to contain only cards that have a reaction with the given card"
  (let ((other-cards (remove card cards)) ;; a card cannot react with itself
        (compatible-card-types (compatible-card-types (type card))))
    (remove-if-not #'(lambda (card)
                       (member (type card) compatible-card-types :test #'eql))
                   other-cards)))

(defun cards-compatible-p (card-1 card-2)
  "Wrapper to card-types-compatible-p to work with real cards"
  (card-types-compatible-p (type card-1) (type card-2)))

(defun react-cards (card-1 card-2)
  (when (cards-compatible-p card-1 card-2)
    (let* ((reaction (get-reaction (type card-1) (type card-2)))
           (action-fun (action reaction)))
      ;; this line is crucial; need a clear interface for what reaction functions take in
      (format t "Reaction between ~a and ~a! Running action ~a...~%" card-1 card-2 action-fun)
      (funcall action-fun (x (position-of card-1)) (y (position-of card-2))))
    (destroy-card card-1)
    (destroy-card card-2)))

(defmacro defreaction (card-1 card-2 &body body)
  `(make-reaction ,card-1 ,card-2 #'(lambda (x y) ,@body)))

(defmacro def-simple-reaction (card-1 card-2 results)
  `(defreaction ,card-1 ,card-2 
     (spawn-cards ,results x y)))
