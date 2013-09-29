(defpackage :lights-out-gui
  (:use :cl :lights-out))

(in-package :lights-out-gui)

(defun draw-light (x y width height color)
  (sdl:draw-box-* x y width height :color color))

(defclass game-state ()
  ((active :accessor active
           :initarg :active
           :initform nil)))

(defmethod shutdown ((state game-state)))

(defclass stage ()
  ((game-state :accessor game-state
               :initarg :game-state)))

(defmethod select-stage ((state game-state) (selected stage))
  (setf (game-state selected) state)
  (setf (active state) selected))

(defmethod select-stage ((selector stage) (selected stage))
  (select-stage (game-state selector) selected))

(defmethod key-down ((stage stage) key))
(defmethod key-up ((stage stage) key))
(defmethod draw ((stage stage)))
(defmethod tick ((stage stage)))

(defclass lights-game (game-state)
  ())

(defclass lights-field (stage)
  ((field :initarg :field
          :accessor field)
   (height-regions :accessor height-regions
                   :initform nil)
   (width-regions :accessor width-regions
                  :initform nil)))

(defmethod key-down ((f lights-field) key)
  (print key)
  (when (sdl:key= key :sdl-key-down)
    (with-slots (columns rows lights) (field f)
      (loop for row from 0 below rows
         do (loop for col from 0 below columns
               do (setf (aref lights row col) nil))))))

(defmethod mouse-down ((lfield lights-field) button x y)
  (let ((column 0)
        (row 0))
    (loop for i from 0 below (first (array-dimensions (width-regions lfield)))
       when (> (aref (width-regions lfield) i) x)
       return (setf row i))
    (loop for i from 0 below (first (array-dimensions (height-regions lfield)))
       when (> (aref (height-regions lfield) i) y)
       return (setf column i))
    (toggle-light column row (field lfield))))

(defmethod draw ((lfield lights-field))
  (with-slots (field) lfield
    (let ((width (truncate (/ *width* (columns field))))
          (height (truncate (/ *height* (rows field)))))
      (unless (height-regions lfield)
        (setf (height-regions lfield) (make-array (list (rows field)) :element-type 'fixnum)))
      (unless (width-regions lfield)
        (setf (width-regions lfield) (make-array (list (columns field)) :element-type 'fixnum)))
      (loop for row from 0 below (rows field)
         do (setf (aref (height-regions lfield) row) (* (1+ row) height)))
      (loop for column from 0 below (columns field)
         do (setf (aref (width-regions lfield) column) (* (1+ column) width)))
      (loop for row from 0 below (rows field)
         do (loop for column from 0 below (columns field)
               do (draw-light (* column width) (* row height) width height (if (aref (lights field) row column)
                                                                               sdl:*yellow*
                                                                               sdl:*blue*)))))))

(defclass winning (stage) ())

(defvar *width*)
(defvar *height*)

(defun start-game (rows columns)
  (let* ((*width* 640)
         (*height* 480)
         (game-state (make-instance 'lights-game))
         (field (make-instance 'lights-out :rows rows :columns columns))
         (gui-field (make-instance 'lights-field :game-state game-state :field field))
         (winning (make-instance 'winning :game-state game-state)))
    (select-stage game-state gui-field)
    (sdl:with-init ()
      (sdl:initialise-default-font)
      (sdl:window *width* *height*)
      (setf (sdl:frame-rate) 30)
      (sdl:with-events (:poll)
        (:quit-event () (progn
                          (shutdown game-state)
                          t))
        (:key-down-event (:key key)
                         (key-down (active game-state) key))
        (:key-up-event (:key key)
                       (key-up (active game-state) key))
        (:mouse-button-down-event (:button button
                                           :x x
                                           :y y)
                                  (mouse-down (active game-state) button x y))
        (:idle ()
               (sdl:clear-display sdl:*black*)
               (draw (active game-state))
               (when (and (not (eq (active game-state) winning))
                          (won? field))
                 (select-stage game-state winning))
               (sdl:update-display))
        (:video-expose-event () (sdl:update-display))))))
