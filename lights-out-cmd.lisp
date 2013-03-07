(defpackage :lights-out-cmd
  (:use :cl :lights-out))

(in-package :lights-out-cmd)

(defmethod draw-lights (stream (g lights-out))
  (with-accessors ((rows rows) (columns columns) (lights lights)) g
    (let ((xwidth (1+ (truncate (log rows 10)))))
      (format stream "~& ")
      (loop for i from 0 below columns
         do (loop repeat xwidth
               do (format stream " "))
         do (format stream "~a" i))
      (loop for x from 0 below rows
         do (format stream "~&~a" x)
         do (loop for y from 0 below columns
               do (loop repeat (- xwidth (if (zerop x)
                                             0
                                             (if (zerop y)
                                                 (truncate (log x 10))
                                                 0)))
                     do (format stream " "))
               do (format stream "~a" (if (aref lights x y)
                                          "#"
                                          " ")))))))

(defun play-game (rows columns)
  (format *query-io* "Starting lights out game.~%")
  (let ((g (make-instance 'lights-out :rows rows :columns columns)))
    (loop
       (draw-lights *query-io* g)
       (when (won? g)
         (return-from play-game "You won."))
       (format *query-io* "~&Enter switch: ")
       (let ((in (read-line *query-io*)))
         (cond ((equalp "quit" in) (return-from play-game))
               (t (handler-case (let ((x (parse-integer (subseq in 0 (position #\, in))))
                                      (y (parse-integer (subseq in (1+ (position #\, in))))))
                                  (toggle-light x y g))
                    (type-error () (format *query-io* "~&Input like this: 5,3"))
                    (sb-int::simple-parse-error () (format *query-io* "~&Input like this: 5,3")))))))))
