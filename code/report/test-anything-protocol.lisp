;;;; test-anything-protocol.lisp --- TODO.
;;;;
;;;; Copyright (C) 2013-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;;; Based on https://testanything.org/

(cl:in-package #:test.report.report)

(defclass tap ()
  ((%version :initarg  :version
             :type     positive-integer
             :reader   version
             :documentation
             ""))
  (:default-initargs
   :version 12)
  (:documentation
   "TODO"))

(register-provider/class 'style :tap :class 'tap)

(defvar *case-number* 0
  "Counter used in these printing functions.")

(defmethod report-using-kind ((kind   (eql :suite))
                              (result t)
                              (style  tap)
                              (target stream))
  (fresh-line target)
  (pprint-logical-block (target nil)
    (let+ (((&accessors-r/o version) style)
           ((&accessors-r/o (descendants model:descendants)) result)
          (total         (length (remove-if #'model:children descendants)))
          (*case-number* 0))
      (unless (zerop total)
        (format target "TAP version ~D~:@_1..~D" version total)
        (mapc (rcurry #'report style target)
              (remove result descendants))))))

(defmethod report-using-kind ((kind   (eql :case))
                              (result t)
                              (style  tap)
                              (target stream))
  (let+ (((&accessors-r/o (name model:name) (descendants model:descendants)) result)
         (cases      (remove :case (remove result descendants) ; TODO :include-self?
                             :key      #'model:kind
                             :test-not #'eq))
         (assertions (remove :case (remove result descendants)
                             :key #'model:kind)))
    (dolist (assertion assertions)
      (let ((status (model:status assertion))
            (number (incf *case-number*)))
        (format target "~0I~:@_~A ~A~@[ - ~A~]~@[ # skip~]"
                (case status
                  (:passed "ok")
                  (t       "not ok"))
                number
                (model:description result)
                (case status
                  (:skipped )))
        (case status
          (:passed)
          (t
           (pprint-newline :mandatory target)
           (pprint-logical-block (target nil :per-line-prefix "#  ")
             (format target "Suite: ~{~S~^ -> ~}"
                     (mapcar #'model:name
                             (butlast (reverse (model:ancestors result)))))
             (format target "~:@_Test: ~S~:@_~:W~:@_" name assertion))))))
    #+no (mapc (rcurry #'report style target) cases)))

(defmethod report-using-kind ((kind   (eql :assertion))
                              (result t)
                              (style  tap)
                              (target stream)))

#+no (defmethod print-format ((condition assertion-error) (format tap) stream)
  (pprint-logical-block (stream nil)
    (with-slots (message) condition
      (format stream "~A" message))))


#+no (defmethod print-format ((condition assertion-failed) (format tap) stream)
  (pprint-logical-block (stream nil)
    (with-slots (expression expected returned forms) condition
      (format stream "Expression: ~S~:@_Expected: ~A~:@_Returned: ~A~{~^~:@_~:[~A~;~S => ~S~]~}"
              expression expected returned forms))))


#+no (defmethod print-format ((condition assertion-fail-forced) (format tap) stream)
  (pprint-logical-block (stream nil)
    (with-slots (format-string args) condition
      (format stream "~?" format-string args))))
