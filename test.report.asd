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
  :depends-on  (:alexandria
                (:version :let-plus                      "0.2")
                (:version :more-conditions               "0.4")
                (:version :utilities.print-items         "0.1")
                (:version :architecture.service-provider "0.1")

                :user-interface.progress)

  :components  ((:module     "model"
                 :pathname   "src/model"
                 :components ((:file       "package")
                              (:file       "types")
                              (:file       "protocol")
                              (:file       "mixins")))

                (:module     "progress" ; TODO where to put this?
                 :pathname   "src/progress"
                 :depends-on ("model")
                 :components ((:file       "fiveam")))

                (:module     "report"
                 :pathname   "src/report"
                 :depends-on ("model")
                 :serial     t
                 :components ((:file       "package")
                              (:file       "protocol")

                              (:file       "default")
                              (:file       "simple")
                              (:file       "test-anything-protocol"))))

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

  :components  ((:module     "test"
                 :serial     t
                 :components ((:file       "package")))))

(defmethod perform ((operation test-op)
                    (component (eql (find-system :test.report/test))))
  (uiop:symbol-call '#:test.report.test '#:run-tests))
