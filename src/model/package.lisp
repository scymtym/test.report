;;;; package.lisp --- Package definition for the model module.
;;;;
;;;; Copyright (C) 2013-2019 Jan Moringen
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
   #:kind
   #:name
   #:status
   #:description)

  ;; Result hierarchy protocol
  (:export
   #:parent
   #:ancestors
   #:children
   #:descendants
   #:count-status)

  (:documentation
   "TODO"))
