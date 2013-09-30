(defpackage :lights-out-qt
  (:use :cl :lights-out :qt))
(in-package :lights-out-qt)

(named-readtables:in-readtable :qt)

;;; works only with compilation
(defmacro relative-file (name)
  (if (or *load-truename*
          *compile-file-truename*)
      (princ-to-string
       (truename
        (make-pathname
         :name (eval name)
         :directory
         (pathname-directory
          (or *load-truename*
              *compile-file-truename*)))))
      (princ-to-string name)))

(defvar *field*)

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

(defclass light ()
  ((size :initarg :size
         :accessor size)
   (icon :initarg :icon
         :accessor icon))
  (:metaclass qt-class)
  (:qt-superclass "QPushButton")
  (:slots ("toggle()" toggle)))

(defmethod field-toggle ((f gui-field) (l light))
  (destructuring-bind (row column) (gethash l (lights-mapping f))
    (toggle-light row column (field f)))
  (redraw f))

(defmethod redraw ((f gui-field))
  (destructuring-bind (rows columns) (array-dimensions (lights (field f)))
    (loop for row from 0 below rows
       do (loop for column from 0 below columns
             do (#_setChecked (#_widget (#_itemAtPosition f row column)) (aref (lights (field f)) row column))))))

(defmethod toggle ((l light))
  (field-toggle *field* l))

(defmethod initialize-instance :after ((l light) &rest rest)
  (declare (ignore rest))
  (new l)
  (#_setCheckable l t)
  (#_setIcon l (icon l))
  (#_setIconSize l (size l)))

(defun lights-out-qt ()
  (let* ((app (make-qapplication))
         (rows 5)
         (columns 5)
         (game (make-instance 'lights-out :rows rows :columns columns))
         (lights (list)))
    (with-objects ((window (#_new QWidget))
                   (*field* (make-instance 'gui-field
                                           :field game))
                   (quit (#_new QPushButton "Quit"))
                   (meta-layout (#_new QVBoxLayout))
                   (quit-layout (#_new QHBoxLayout))
                   (icon (#_new QIcon))
                   (icon-size (#_new QSize 65 65))
                   (off (#_new QImage #.(relative-file "Off.jpg")))
                   (on (#_new QImage #.(relative-file "On.jpg")))
                   (offpix (#_QPixmap::fromImage off))
                   (onpix (#_QPixmap::fromImage on)))
      (#_setWindowTitle window "Lights Out")
      (#_addPixmap icon
                   onpix
                   (#_QIcon::Active)
                   (#_QIcon::On))
      (#_addPixmap icon
                   offpix
                   (#_QIcon::Active)
                   (#_QIcon::Off))
      (#_connect "QObject"
                 quit (QSIGNAL "clicked()")
                 app (QSLOT "quit()"))
      (loop for row from 0 below rows
         do (loop for column from 0 below columns
               do (let ((light (make-instance 'light :size icon-size :icon icon)))
                    (push light lights)
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
        (progn (dolist (light lights)
                 (qt::%maybe-delete light))
               (#_hide window))))))
