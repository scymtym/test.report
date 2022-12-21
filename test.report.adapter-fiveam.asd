;;;; test.report.adapter-fiveam.asd --- System definition for test.report.adapter-fiveam.
;;;;
;;;; Copyright (C) 2010-2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "test.report.adapter-fiveam"
  :description "TODO"
  :license     "GPLv3" ; see COPYING file for details
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("alexandria"
                (:version "let-plus"                      "0.2")
                (:version "more-conditions"               "0.4")
                (:version "utilities.print-items"         "0.1")
                (:version "architecture.service-provider" "0.1")

                (:version "fiveam"                        "1.4")

                (:version "test.report"                   (:read-file-form "version-string.sexp")))

  :components  ((:module     "fiveam"
                 :pathname   "code/adapter"
                 :components ((:file       "fiveam"))))

  :in-order-to ((test-op (test-op "test.report.adapter-fiveam/test"))))

(defsystem "test.report.adapter-fiveam/test"
  :description "Unit tests for the test.report.adapter-fiveam system."
  :license     "GPLv3" ; see COPYING file for details

  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ((:version "test.report" (:read-file-form "version-string.sexp"))
                (:version "fiveam"      "1.4"))

  :components  ((:module     "test"
                 :serial     t
                 :components ((:file       "package"))))

  :perform     (test-op (operation component)
                 (uiop:symbol-call '#:test.report.test '#:run-tests)))
