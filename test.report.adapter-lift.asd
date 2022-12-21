;;;; test.report.adapter-lift.asd --- System definition for test.report.adapter-lift.
;;;;
;;;; Copyright (C) 2010-2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "test.report.adapter-lift"
  :description "Adapter for reporting results of lift tests."
  :license     "GPLv3" ; see COPYING file for details
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  (:alexandria
                (:version "let-plus"                      "0.2")
                (:version "more-conditions"               "0.4")
                (:version "utilities.print-items"         "0.3")
                (:version "architecture.service-provider" "0.1")

                (:version "lift"                          "1.3")

                (:version "test.report"                   (:read-file-form "version-string.sexp")))

  :components  ((:module     "lift"
                 :pathname   "code/adapter"
                 :components ((:file       "lift"))))

  :in-order-to ((test-op (test-op "test.report.adapter-lift/test"))))

(defsystem "test.report.adapter-lift/test"
  :description "Unit tests for the test.report.adapter-lift system."
  :license     "GPLv3" ; see COPYING file for details
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ((:version "test.report" (:read-file-form "version-string.sexp"))
                (:version "lift"      "1.3"))

  :components  ((:module     "test"
                 :serial     t
                 :components ((:file       "package"))))

  :perform     (test-op (operation component)
                 (uiop:symbol-call '#:test.report.test '#:run-tests)))
