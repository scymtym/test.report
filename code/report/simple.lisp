;;;; simple.lisp --- Simple result reporting style.
;;;;
;;;; Copyright (C) 2013-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;; Note: This code is based on the simple result reporting style of
;;; the fiveam library by Edward Marco Baringer.

(cl:in-package #:test.report.report)

(defclass simple ()
  (
   ; (stream *test-dribble*)
   ; (recursive-depth 0)
   )
  (:documentation
   "TODO

    Note: This style is based on the simple result reporting style of
    the fiveam library by Edward Marco Baringer."))

(register-provider/class 'style :simple :class 'simple)

(defmethod report-using-kind ((kind   (eql :suite))
                              (result t)
                              (style  simple)
                              (target stream))
  (let* ((num-passed   (model:count-status :passed result))
         (num-skipped  (model:count-status :skipped result))
         (num-failures (model:count-status :failure result))
         (num-errors   (model:count-status :error result))
         (num-unknown  0)
         (num-checks   (+ num-passed num-skipped num-failures num-errors num-unknown)))
    (format target "~&~vTRan ~D checks, ~D passed~
                    ~[~:;~:*, ~D skipped~] ~
                    and ~D failed.~%~
                    ~vT~[~:;~:*~D UNKNOWN RESULTS.~%~]"
            0 #+TODO recursive-depth num-checks num-passed num-skipped num-failures
            0 #+TODO recursive-depth num-unknown)))
