;;;; test.report.report-junit.asd --- System definition for test.report.report-junit.
;;;;
;;;; Copyright (C) 2010-2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "test.report.report-junit"
  :description "Write test report in JUnit format"
  :license     "GPLv3" ; see COPYING file for details
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("alexandria"
                (:version "let-plus"                      "0.2")
                (:version "more-conditions"               "0.4")
                (:version "utilities.print-items"         "0.1")
                (:version "architecture.service-provider" "0.1")

                "cxml"

                (:version "test.report"                   (:read-file-form "version-string.sexp")))

  :components  ((:module     "report"
                 :pathname   "code/report"
                 :serial     t
                 :components ((:file       "junit"))))

  :in-order-to ((test-op (test-op "test.report.report-junit/test"))))

(defsystem "test.report.report-junit/test"
  :description "Unit tests for the test.report.report-junit system."
  :license     "GPLv3" ; see COPYING file for details
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ((:version "test.report" (:read-file-form "version-string.sexp"))
                (:version "fiveam"      "1.4"))

  :components  ()

  :perform     (test-op (operation component)
                 (uiop:symbol-call '#:test.report.test '#:run-tests)))
