;;;; protocol.lisp --- Protocol provided by the model module.
;;;;
;;;; Copyright (C) 2013-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.DE>

(cl:in-package #:test.report.model)

;;; Result protocol

(defgeneric kind (result)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric name (result)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric status (result)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric description (result)
  (:documentation
   "TODO(jmoringe): document"))

;;; Result hierarchy protocol

(defgeneric parent (result)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric ancestors (result)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric children (result)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric descendants (result) ; TODO &key include-self?
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric count-status (status result)
  (:documentation
   "TODO(jmoringe): document"))

;; TODO duration
;; TODO memory
;; TODO (result-resource-consumption result :memory)?

;; Default behavior

(defmethod parent ((result t))
  nil)

(defmethod ancestors ((result t))
  (list* result (when-let ((parent (parent result)))
                  (ancestors parent))))

(defmethod children ((result t))
  '())

(defmethod descendants ((result t)) ; TODO &key include-self?
  (list* result (mappend #'descendants (children result))))

(defmethod count-status ((status t) (result t))
  (if (eq (status result) status) 1 0))

;;;

(defun has-status (status &rest more-statuses)
  (lambda (test)
    (member (status test) (list* status more-statuses))))
