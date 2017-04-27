;;;; protocol.lisp --- Protocol provided by the model module.
;;;;
;;;; Copyright (C) 2013, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.DE>

(cl:in-package #:test.result.model)

;;; Result protocol

(defgeneric result-kind (result)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric result-name (result)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric result-status (result)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric result-description (result)
  (:documentation
   "TODO(jmoringe): document"))

;;; Result hierarchy protocol

(defgeneric result-parent (result)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric result-ancestors (result)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric result-children (result)
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric result-descendants (result) ; TODO &key include-self?
  (:documentation
   "TODO(jmoringe): document"))

(defgeneric count-status (status result)
  (:documentation
   "TODO(jmoringe): document"))

;; TODO duration
;; TODO memory
;; TODO (result-resource-consumption result :memory)?

;; Default behavior

(defmethod result-parent ((result t))
  nil)

(defmethod result-ancestors ((result t))
  (list* result (when-let ((parent (result-parent result)))
                  (result-ancestors parent))))

(defmethod result-children ((result t))
  '())

(defmethod result-descendants ((result t)) ; TODO &key include-self?
  (list* result (mappend #'result-descendants (result-children result))))

(defmethod count-status ((status t) (result t))
  (if (eq (result-status result) status) 1 0))

;;;

(defun has-status (status &rest more-statuses)
  (lambda (test)
    (member (result-status test) (list* status more-statuses))))
