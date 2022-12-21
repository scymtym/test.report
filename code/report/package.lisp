;;;; package.lisp --- Package definition for the report module.
;;;;
;;;; Copyright (C) 2013-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:test.report.report
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:more-conditions

   #:service-provider)

  (:local-nicknames
   (#:model #:test.report.model))

  ;; Report protocol
  (:export
   #:report
   #:report-using-kind)

  (:documentation
   "Protocols and implementations for generating reports for unit test
    results."))
