;;;; test.report.asd --- System definition for test.report.
;;;;
;;;; Copyright (C) 2010-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem :test.report
  :description "TODO"
  :license     "LLGPLv3" ; see COPYING file for details

  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ()

  :components  ((:static-file "COPYING")
                (:static-file "README.org"))

  :in-order-to ((test-op (test-op :test.report/test))))

(defsystem :test.report/test
  :description "Unit tests for the test.report system."
  :license     "LLGPLv3" ; see COPYING file for detail

  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ((:version :test.report (:read-file-form "version-string.sexp"))
                (:version :fiveam      "1.3"))

  :components  ())
