;;;; fiveam.lisp --- Adapter for the fiveam framework.
;;;;
;;;; Copyright (C) 2013, 2014, 2017, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.DE>

(cl:defpackage #:test.report.adapter.fiveam
  (:use
   #:cl
   #:alexandria
   #:let-plus)

  (:local-nicknames
   (#:model #:test.report.model))

  (:export
   #:import-results)

  (:export
   #:hook-into-run))

(cl:in-package #:test.report.adapter.fiveam)

;;; Wrapper for fiveam assertions
;;;
;;; This is needed to make the hierarchy functions work.

(defclass 5am-wrapper (model::assertion-result)
  ((%assertion :initarg  :assertion
               :reader   assertion
               :documentation
               "Stores the wrapped assertion result."))
  (:documentation
   "Wraps fiveam results to make the hierarchy work."))

(defmethod model:name ((result 5am-wrapper))
  (5am::name (5am::test-case (assertion result))))

(defmethod model:status ((result 5am-wrapper))
  (model:status (assertion result)))

(defmethod model:status ((result 5am::test-passed))
  :passed)

(defmethod model:status ((result 5am::test-skipped))
  :skipped)

(defmethod model:status ((result 5am::test-failure))
  :failure)

(defmethod model:description ((result 5am-wrapper))
  (5am::reason (assertion result)))

;;; Import

(defun import-results (results &key suite)
  "Import RESULTS into a suite, case, assertion tree structure.

   RESULTS has to be a list of fiveam results."
  (let+ ((original->result (make-hash-table :test #'eq))
         ((&flet add-case (original parent)
            (ensure-gethash
             original original->result
             (let ((case-result (make-instance 'model::test-case-result
                                               :name   (5am::name original)
                                               :parent parent)))
               (appendf (model::%children parent)
                        (list case-result))
               case-result))))
         ((&labels walk-suites (suite &optional parent)
            (let ((suite-result (apply #'make-instance 'model::test-suite-result
                                       :name (5am::name suite)
                                       (when parent
                                         (list :parent parent)))))
              (map nil (compose
                        (lambda (child)
                          (if (typep child '5am::test-suite)
                              (walk-suites child suite-result)
                              (add-case child suite-result)))
                        #'5am:get-test)
                   (hash-table-values (5am::tests suite)))
              suite-result))))
    (let ((suite-result (if suite
                            (walk-suites suite)
                            )))
      (map nil (lambda (result)
                 (let* ((case        (5am::test-case result))
                        (case-result (gethash case original->result)))
                   (appendf (model::%children case-result)
                            (list (make-instance '5am-wrapper
                                                 :assertion result
                                                 :parent    case-result)))))
           results)
      suite-result)))

;;;

(defun hook-into-run ()
  (sb-int:encapsulate
   'fiveam:run 'test-report
   (lambda (function test-spec &rest args)
     (let* ((target      (make-pathname :name (format nil "test-results-~(~A~)"
                                                 (string test-spec))
                                   :type "xml"))
            (suite       (fiveam:get-test test-spec))
            (raw-results (apply function test-spec args))
            (results     (import-results raw-results :suite suite)))
       (test.report.report:report results :junit target)
       raw-results))))
