;;;; package.lisp --- Package definition for the model module.
;;;;
;;;; Copyright (C) 2013, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:test.report.model
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:more-conditions)

  ;; Types
  (:export
   #:kind

   #:status
   #:composite-status)

  ;; Result protocol
  (:export
   #:result-kind
   #:result-name
   #:result-status
   #:result-description)

  ;; Result hierarchy protocol
  (:export
   #:result-parent
   #:result-ancestors
   #:result-children
   #:result-descendants
   #:count-status)

  (:documentation
   "TODO"))
