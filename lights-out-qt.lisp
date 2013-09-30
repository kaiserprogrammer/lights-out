(defpackage :lights-out-qt
  (:use :cl :lights-out :qt))
(in-package :lights-out-qt)

(named-readtables:in-readtable :qt)

(defvar *field*)
(defvar *game*)

(defclass gui-field ()
  ((lights-mapping :accessor lights-mapping)
   (field :initarg :field
          :accessor field))
  (:metaclass qt-class)
  (:qt-superclass "QGridLayout"))

(defmethod initialize-instance :after ((f gui-field) &rest rest)
  (declare (ignore rest))
  (new f)
  (setf (lights-mapping f) (make-hash-table)))

(defmethod field-toggle ((f gui-field) (l light))
  (destructuring-bind (row column) (gethash l (lights-mapping f))
    (toggle-light row column (field f)))
  (redraw f))

(defmethod redraw ((f gui-field))
  (destructuring-bind (rows columns) (array-dimensions (lights (field f)))
    (loop for row from 0 below rows
       do (loop for column from 0 below columns
             do (#_setChecked (#_widget (#_itemAtPosition f row column)) (aref (lights (field f)) row column))))))

(defclass light ()
  ()
  (:metaclass qt-class)
  (:qt-superclass "QPushButton")
  (:slots ("toggle()" toggle)))

(defmethod toggle ((l light))
  (field-toggle *field* l))

(defmethod initialize-instance :after ((l light) &rest rest)
  (declare (ignore rest))
  (new l)
  (#_setCheckable l t)
  (let ((icon (#_new QIcon)))
    (#_addPixmap icon
                 (#_QPixmap::fromImage(#_new QImage "/home/coder/Pictures/cucon/AAlarm_1.jpg"))
                 (#_QIcon::Active)
                 (#_QIcon::On))
    (#_addPixmap icon
                 (#_QPixmap::fromImage(#_new QImage "/home/coder/Pictures/cucon/Inactive.jpg"))
                 (#_QIcon::Active)
                 (#_QIcon::Off))
    (#_setIcon l icon))
  (#_setIconSize l (#_new QSize 65 65)))

(defun lights-out-qt ()
  (declare (optimize (debug 3)))
  (let* ((app (make-qapplication))
         (window (#_new QWidget))
         (rows 5)
         (columns 5)
         (game (make-instance 'lights-out :rows rows :columns columns))
         (*field* (make-instance 'gui-field
                                 :field game))
         (quit (#_new QPushButton "Quit"))
         (meta-layout (#_new QVBoxLayout))
         (quit-layout (#_new QHBoxLayout)))
    (#_setWindowTitle window "Lights Out")
    (#_connect "QObject"
               quit (QSIGNAL "clicked()")
               app (QSLOT "quit()"))
    (loop for row from 0 below rows
       do (loop for column from 0 below columns
             do (let ((light (make-instance 'light)))
                  (#_addWidget *field* light row column)
                  (setf (gethash light (lights-mapping *field*)) (list row column))
                  (#_connect "QObject"
                             light (QSIGNAL "clicked()")
                             light (QSLOT "toggle()")))))
    (redraw *field*)
    (#_addWidget quit-layout quit)
    (#_insertStretch quit-layout 1)
    (#_addLayout meta-layout *field*)
    (#_addLayout meta-layout quit-layout)
    (#_setSizeConstraint meta-layout (#_QLayout::SetFixedSize))
    (#_setLayout window meta-layout)
    (#_show window)
    (unwind-protect
         (#_exec app)
      (#_hide window))))