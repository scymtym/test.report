;;;; junit.lisp --- JUnit output.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:test.report)

(defclass junit ()
  ((indentation :initarg  :indentation
                :reader   style-indentation
                :initform 2))
  (:documentation
   "TODO(jmoringe): document"))

(register-provider/class 'style :junit
                         :class 'junit)

(defvar *root-element-emitted?* nil)

(defmethod report-using-kind ((kind   (eql :suite))
                              (result t)
                              (style  junit)
                              (target stream))
  (let+ (((&structure-r/o result- name children) result)
         ((&structure-r/o style- indentation) style)
         ((num-passed num-failures num-errors)
          (mapcar (rcurry #'count-status result) '(:passed :failure :error)))
         ((&flet emit-content ()
            (cxml:with-element "testsuite"
              (cxml:attribute "id"        (%make-compatible-name (string name)))
              (cxml:attribute "name"      (string (symbol-name name)))
              (cxml:attribute "package"   (string (package-name (symbol-package name))))
              (cxml:attribute "hostname"  (machine-instance))

              (cxml:attribute "tests"     (+ num-passed num-failures num-errors))
              (cxml:attribute "errors"    num-errors)
              (cxml:attribute "failures"  num-failures)

              #+no (cxml:attribute "time"      (format nil "~F" time))
              #+no (when (getf (third (first tests)) :start-time-universal)
                     (cxml:attribute "timestamp" (local-time:format-timestring
                                                  nil (local-time:universal-to-timestamp
                                                       (getf (third (first tests)) :start-time-universal)))))

              (cxml:with-element "properties"
                #+not-used (cxml:with-element "property"
                             (cxml:attribute "name" "NAME")
                             (cxml:attribute "value" "VALUE")))

              (map nil (rcurry #'report style target) children)

              (cxml:with-element "system-out")
              (cxml:with-element "system-err")))))
    (if *root-element-emitted?*
        (emit-content)
        (cxml:with-xml-output (cxml:make-character-stream-sink
                               target
                               :indentation indentation
                               :canonical   nil)
          (cxml:with-element "testsuites"
            (let ((*root-element-emitted?* t))
              (emit-content)))))))

(defmethod report-using-kind ((kind   (eql :case))
                              (result t)
                              (style  junit)
                              (target stream))
  (let+ (((&structure-r/o result- name status children) result))
    (cxml:with-element "testcase"
      (cxml:attribute "name" (string name))
      #+no (cxml:attribute "classname" (%make-compatible-name
                                        (prin1-to-string suite)))
      #+no (cxml:attribute "time"      (format nil "~F" (getf data :seconds)))
      (if (and (member status '(:failure :error))
               (emptyp children))
          (%junit-emit-failure-element result)
          (map nil (rcurry #'report style target) children)))))

(defmethod report-using-kind ((kind   (eql :assertion))
                              (result t)
                              (style  junit)
                              (target stream))
  (when (member (result-status result) '(:failure :error))
    (%junit-emit-failure-element result)))

(defun %junit-emit-failure-element (result)
  "TODO(jmoringe): document"
  (let+ (((&structure-r/o result- status description) result))
    (cxml:with-element (coerce (string-downcase status) '(simple-array character (*))) ; TODO cxml kludge
      (cxml:attribute "type" (ecase status
                               (:failure "failure")
                               (:error   "error"))) ; TODO should be condition class name, if any
      (cxml:attribute "message" description)
      (when-let ((backtrace nil #+no (result-backtrace result)))
        (cxml:text backtrace)))))

(defun %make-compatible-name (name)
  (flet ((remove-double-colon (name)
           (if-let ((position (search "::" name)))
             (remove #\: name :start position :count 1)
             name))
         (colon->dot (name)
           (substitute #\. #\: name)))
    (colon->dot (remove-double-colon name))))
