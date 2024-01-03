;;;; mixins.lisp --- Mixins used by the model module.
;;;;
;;;; Copyright (C) 2013-2022, 2024 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.DE>

(cl:in-package #:test.report.model)

;;; `kind-mixin'

(defclass kind-mixin ()
  ((%kind :initarg  :kind
          :type     kind
          :reader   kind
          :documentation
          ""))
  (:default-initargs
   :kind (missing-required-initarg 'kind-mixin :kind))
  (:documentation
   "TODO(jmoringe): document"))

;;; `named-mixin'

(defclass named-mixin ()
  ((%name :initarg  :name
          :reader   name
          :documentation
          ""))
  (:default-initargs
   :name (missing-required-initarg 'named-mixin :name))
  (:documentation
   "TODO"))

(defmethod print-items:print-items append ((object named-mixin))
  `(((:name (:before :status)) "~A" ,(name object))))

(defmethod description ((result named-mixin))
  (format nil "~A ~A" (kind result) (name result)))

;;; `parented-mixin'

(defclass parented-mixin ()
  ((%parent :initarg  :parent
            :reader   parent
            :initform nil
            :documentation
            ""))
  (:documentation
   "TODO"))

(defmethod description ((result parented-mixin))
  (format nil "~A ~{~A~^.~}"
          (kind result)
          (mapcar #'name (ancestors result))))

;;; `status-mixin'

(defclass status-mixin ()
  ((%status :initarg  :status
            ;;:type     status
            :reader   status
            :documentation
            ""))
  (:documentation
   "TODO"))

(defmethod print-items:print-items append ((object status-mixin))
  `(((:status (:after :name)) "~A" ,(status object))))

;;; `composite-result-mixin'

(defclass composite-result-mixin ()
  ((%children :initarg  :children
              :type     vector
              :reader   children
              :accessor %children
              :initform (make-array 0 :adjustable t :fill-pointer 0)
              :documentation
              ""))
  (:documentation
   "TODO"))

(defmethod print-items:print-items append ((object composite-result-mixin))
  `(((:num-children (:after :name))         " (~D)" ,(length (children object)))
    ((:status       (:after :num-children)) " ~A"   ,(status object))))

(defmethod status ((test composite-result-mixin))
  (let ((children (children test)))
    (cond ((some (has-status :error) children)
           :error)
          ((some (has-status :failure) children)
           :failure)
          ((every (has-status :skipped) children)
           :skipped)
          ((every (has-status :passed) children)
           :passed)
          (t
           :mixed))))

(defmethod count-status ((status t) (result composite-result-mixin))
  (reduce #'+ (children result) :key (curry #'count-status status)))

;;; TODO move these somewhere else

(defclass test-suite-result (kind-mixin
                             named-mixin
                             parented-mixin
                             composite-result-mixin
                             print-items:print-items-mixin)
  ()
  (:default-initargs
   :kind :suite))

(defclass test-case-result (kind-mixin
                            named-mixin
                            parented-mixin
                            composite-result-mixin
                            print-items:print-items-mixin)
  ()
  (:default-initargs
   :kind :case))

(defclass assertion-result (kind-mixin
                            parented-mixin
                            print-items:print-items-mixin)
  ()
  (:default-initargs
   :kind :assertion))
