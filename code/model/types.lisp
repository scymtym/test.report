;;;; types.lisp --- Types used in the model module.
;;;;
;;;; Copyright (C) 2013-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.DE>

(cl:in-package #:test.report.model)

(deftype kind ()
  "TODO"
  '(member :suite :case :assertion))

(deftype status () ; TODO renamed to singleton-status?
  "TODO"
  '(member :failure :skipped :passed))

(deftype composite-status ()
  "TODO"
  '(or status (member :mixed)))
