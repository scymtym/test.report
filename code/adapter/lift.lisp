;;;; lift.lisp --- Result adapter for the lift framework.
;;;;
;;;; Copyright (C) 2013-2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.DE>

(cl:defpackage #:test.report.adapter.lift
  (:use
   #:cl
   #:alexandria
   #:let-plus)

  (:local-nicknames
   (#:model #:test.report.model))

  (:export
   #:import-results))

(cl:in-package #:test.report.adapter.lift)

(defun handle-lift-case (name parent tests)
  (let ((case (make-instance 'lift-suite-result-wrapper
                             :name   name
                             :parent parent
                             :children '())))
    (reinitialize-instance case :children (mapcar (lambda (test)
                                                    (make-instance 'lift-case-result-wrapper :data test :parent case))
                                                  tests))))

(defun handle-lift-result (result)
  (let ((tests (lift::tests-run result))
        (root  (make-instance 'lift-suite-result-wrapper
                              :name :root
                              :children '())))
    (reinitialize-instance root :children (mapcar (lambda (name)
                                                    (handle-lift-case name root (remove name tests
                                                                                        :key      #'first
                                                                                        :test-not #'eq)))
                                                  (lift::suites-run result)))))

;;; test suite

(defclass lift-suite-result-wrapper (model::test-suite-result)
  ()
  (:documentation
   "TODO(jmoringe): document"))

;;; test case

(defclass lift-case-result-wrapper (model::kind-mixin
                                    model::parented-mixin
                                    print-items:print-items-mixin)
  ((data :initarg  :data
         :type     list
         :reader   result-%data
         :documentation
         ""))
  (:default-initargs
   :kind :case)
  (:documentation
   "TODO(jmoringe): document"))

(defmethod print-items:print-items append ((object lift-case-result-wrapper))
  `(((:name   (:before :status)) "~A" ,(result-name object)  )
    ((:status (:after :name))    " ~A" ,(result-status object))))

(defmethod result-%plist ((result lift-case-result-wrapper))
  (third (result-%data result)))

(defmethod result-%property ((result lift-case-result-wrapper) (key t)
                             &optional default)
  (getf (result-%plist result) key default))

(defmethod result-name ((result lift-case-result-wrapper))
  (second (result-%data result)))

(defmethod result-status ((result lift-case-result-wrapper))
  (etypecase (result-%property result :problem)
    (null                  :passed)
    (lift::test-failure    :failure)
    (lift::test-error      :error) ; TODO
    (lift::testsuite-error :error)))

(defmethod result-description ((result lift-case-result-wrapper))
  (when-let ((condition (result-%property result :problem)))
    (princ-to-string (lift:test-condition condition))))

;;; Assertions

(defmethod result-kind ((result lift::test-problem-mixin))
  :assertion)

(defmethod result-name ((result lift::test-problem-mixin))
  :assertion)
