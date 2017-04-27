;;;; mixins.lisp --- Mixins used by the model module.
;;;;
;;;; Copyright (C) 2013, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.DE>

(cl:in-package #:test.resport.model)

;;; `kind-mixin'

(defclass kind-mixin ()
  ((kind :initarg  :kind
         :type     kind
         :reader   result-kind
         :documentation
         ""))
  (:default-initargs
   :kind (missing-required-initarg 'kind-mixin :kind))
  (:documentation
   "TODO(jmoringe): document"))

;;; `named-mixin'

(defclass named-mixin ()
  ((name :initarg  :name
         :reader   result-name
         :documentation
         ""))
  (:default-initargs
   :name (missing-required-initarg 'named-mixin :name))
  (:documentation
   "TODO"))

(defmethod print-items:print-items append ((object named-mixin))
  `((:name ,(result-name object) "~A" ((:before :status)))))

(defmethod result-description ((result named-mixin))
  (format nil "~A ~A" (result-kind result) (result-name result)))

;;; `parented-mixin'

(defclass parented-mixin ()
  ((parent :initarg  :parent
           :reader   result-parent
           :initform nil
           :documentation
           ""))
  (:documentation
   "TODO"))

(defmethod result-description ((result parented-mixin))
  (format nil "~A ~{~A~^.~}"
          (result-kind result)
          (mapcar #'result-name (result-ancestors result))))

;;; `status-mixin'

(defclass status-mixin ()
  ((status :initarg  :status
           ;:type     status
           :reader   result-status
           :documentation
           ""))
  (:documentation
   "TODO"))

(defmethod print-items:print-items append ((object status-mixin))
  `((:status ,(result-status object) "~A" ((:after :name)))))

;;; `composite-result-mixin'

(defclass composite-result-mixin ()
  ((children :initarg  :children
             :type     list
             :reader   result-children
             :accessor result-%children
             :initform '()
             :documentation
             ""))
  (:documentation
   "TODO"))

(defmethod print-items:print-items append ((object composite-result-mixin))
  `((:num-children ,(length (result-children object)) " (~D)" ((:after :name)))
    (:status       ,(result-status object)            " ~A"   ((:after :num-children)))))

(defmethod result-status ((test composite-result-mixin))
  (let ((children (result-children test)))
    (cond
      ((some (has-status :error) children)
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
  (reduce #'+ (result-children result) :key (curry #'count-status status)))

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