;;;; protocol.lisp --- Protocol provided by the test.report system.
;;;;
;;;; Copyright (C) 2013-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.DE>

(cl:in-package #:test.report.report)

;;; Test result report protocol

(defgeneric report (result style target)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric report-using-kind (kind result style target)
  (:documentation
   "TODO(jmoringe): document"))

;;; Default behavior

(defmethod report ((result t) (style symbol) (target t))
  (report result (list style) target))

(defmethod report ((result t) (style cons) (target t))
  (report result (apply #'make-provider 'style style) target))

(defmethod report ((result t) (style t) (target pathname))
  (with-open-file (stream target :direction         :output
                                 :if-does-not-exist :create
                                 :if-exists         :supersede)
    (report result style target)))

(defmethod report ((result t) (style t) (target t))
  (report-using-kind (model:kind result) result style target))

;;; Test result report style service

(define-service style
  (:documentation
   "Providers of this service present test result in particular ways.

    Provider instances are intended to be used in STYLE role of the
    report protocol (i.e. the `report' and `report-using-kind' generic
    functions)."))
