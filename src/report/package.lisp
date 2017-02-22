;;;; package.lisp --- Package definition for the report module.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:test.report
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:more-conditions

   #:service-provider

   #:test.model)

  ;; Report protocol
  (:export
   #:report
   #:report-using-kind)

  (:documentation
   "This package contains protocols and implementations for generating
    reports for unit test results."))
