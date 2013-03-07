(defpackage :lights-out
  (:use :cl :lisp-unit)
  (:export
   :lights-out
   :light-on?
   :toggle-light
   :set-light
   :rows
   :columns
   :lights
   :won?))

(in-package :lights-out)

(defclass lights-out ()
  ((lights :accessor lights)
   (rows :reader rows
         :initarg :rows
         :initform 3)
   (columns :reader columns
            :initarg :columns
            :initform 3)))

(defmethod initialize-instance :after ((g lights-out) &key)
  (with-slots (rows columns lights) g
    (setf lights
          (make-array (list rows columns) :initial-element t))))

(defmethod toggle-light ((x number) (y number) (g lights-out))
  (when (and (< x (rows g))
             (< y (columns g)))
   (set-toggle x y g)
   (set-toggle (1- x) y g)
   (set-toggle (1+ x) y g)
   (set-toggle x (1- y) g)
   (set-toggle x (1+ y) g)))

(defmethod light-on? ((x number) (y number) (g lights-out))
  (aref (lights g) x y))

(defmethod set-light ((x number) (y number) (g lights-out) val)
  (setf (aref (lights g) x y) val))

(defmethod set-toggle ((x number) (y number) (g lights-out))
  (with-slots (rows columns lights) g
    (when (and (>= x 0)
               (>= y 0)
               (< x rows)
               (< y columns))
     (setf (aref lights x y) (not (aref lights x y))))))

(defmethod won? ((g lights-out))
  (with-slots (rows columns lights) g
    (dotimes (x rows)
      (dotimes (y columns)
        (when (aref lights x y)
          (return-from won? nil))))
    t))

(remove-tests :all)

(define-test lights-out
  (let ((game (make-instance 'lights-out)))
    (toggle-light 1 1 game)
    (assert-equal nil (light-on? 1 1 game))
    (assert-equal nil (light-on? 1 0 game))
    (assert-equal nil (light-on? 0 1 game))
    (assert-equal nil (light-on? 2 1 game))
    (assert-equal nil (light-on? 1 2 game))
    (assert-equal t (light-on? 0 0 game))))

(define-test lights-out-toggle
  (let ((game (make-instance 'lights-out)))
    (set-light 1 0 game nil)
    (toggle-light 1 1 game)
    (assert-equal t (light-on? 1 0 game))))

(define-test lights-out-range
  (let ((game (make-instance 'lights-out :rows 5 :columns 3)))
    (toggle-light 0 0 game)
    (assert-equal t (light-on? 1 1 game))
    (assert-equal nil (light-on? 0 0 game))
    (assert-equal nil (light-on? 1 0 game))
    (assert-equal nil (light-on? 0 1 game))
    (toggle-light 4 2 game)
    (assert-equal t (light-on? 3 1 game))
    (assert-equal nil (light-on? 4 2 game))
    (assert-equal nil (light-on? 4 1 game))
    (assert-equal nil (light-on? 3 2 game))))

(define-test lights-out-winning
  (let ((g (make-instance 'lights-out :rows 2 :columns 1)))
    (assert-false (won? g))
    (set-light 0 0 g nil)
    (assert-false (won? g))
    (set-light 1 0 g nil)
    (assert-true (won? g))))

(let ((*print-failures* t)
      (*print-errors* t))
  (run-tests :all))
