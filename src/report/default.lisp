;;;; default.lisp --- Default result reporting style.
;;;;
;;;; Copyright (C) 2013, 2014, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;;; Note: This code is based on the default result reporting style of
;;;; the fiveam library by Edward Marco Baringer.

(cl:in-package #:test.report)

(defvar *verbose-failures* nil
  "TODO")

(defclass default ()
  ((max-depth        :initarg  :max-depth
                     :type     (or t non-negative-integer)
                     :reader   style-max-depth
                     :documentation
                     "")
   (verbose-failures :initarg  :verbose-failures
                     :type     boolean
                     :reader   style-verbose-failures
                     :documentation
                     "")
   ; (stream *test-dribble*)
   ; (recursive-depth 0)
   )
  (:default-initargs
   :verbose-failures *verbose-failures*)
  (:documentation
   "TODO

    Note: This style is based on the default result reporting style of
    the fiveam library by Edward Marco Baringer."))

(register-provider/class 'style :default
                         :class 'default)

(defmethod report-using-kind ((kind   (eql :suite))
                              (result t)
                              (style  default)
                              (target stream))
  (let+ (((&structure-r/o style- verbose-failures) style)
         ((num-passed num-skipped num-failures num-errors)
          (mapcar (rcurry #'count-status result)
                  '(:passed :skipped :failure :error)))
         (num-failures* (+ num-failures num-errors))
         (num-unknown   0)
         (num-checks    (+ num-passed num-skipped num-failures num-errors num-unknown))
         (assertions    (remove-if (disjoin #'result-children
                                            (of-type 'test.model::test-suite-result)
                                            (of-type 'test.model::test-case-result)) ; TODO
                                   (result-descendants result)))
         ((&flet ratio (number)
            (/ number num-checks)))
         ((&flet output (&rest format-args)
            (format target "~&~vT" 0 #+TODO recursive-depth)
            (apply #'format target format-args)))
         ((&flet rule ()
            (output "--------------------------------~%"))))
    (when (zerop num-checks)
      (output "Didn't run anything...huh?")
      (return-from report-using-kind nil))

    ;; Summary.
    (let ((width (length (format nil "~:D" num-checks))))
      (output "Did ~:D check~:P.~%~
             ~:{~3@T~A: ~V:D (~4,0,2F%)~%~}"
              num-checks
              (list* (list "Pass" width num-passed    (ratio num-passed))
                     (list "Skip" width num-skipped   (ratio num-skipped))
                     (list "Fail" width num-failures* (ratio num-failures*))
                     (when (plusp num-unknown)
                       (list "UNKNOWN RESULTS" width num-unknown (ratio num-unknown))))))
    (terpri target)

    ;; Failure details.
    (when-let ((failed (remove-if (complement (test.model::has-status :failure :error)) ; TODO predicate?
                                  assertions)))
      (output "Failure Details:~%")
      (dolist (assertion failed)
        (rule)
        (output "~A~@[[~{~A~^.~}]~]: ~%~
                 ~5@T~A.~%"
                (result-name assertion)
                (mapcar #'result-name
                        (reverse (result-ancestors (result-parent assertion))))
                (result-description assertion))
        #+no (when (5am::for-all-test-failed-p f)
               (output "Results collected with failure data:~%")
               (explain exp (slot-value f 'result-list)
                        target (+ 4 0 #+TODO recursive-depth)))
        #+no (when (and verbose-failures (test-expr f))
               (output "    ~S~%" (test-expr f)))
        (rule))
      (terpri target))

    (when-let ((skipped (remove-if (complement (test.model::has-status :skipped)) ; TODO package
                                   assertions)))
      (output "Skip Details:~%")
      (dolist (assertion skipped)
        )
      (terpri target))))

(defmethod report-using-kind ((kind   (eql :assertion))
                              (result t)
                              (style  default)
                              (target stream))
  (let+ (((&structure-r/o result- name parent description) result)
         (parent-description (result-description parent)))
    (format target "--------------------------------~%~
                    ~A~@[[~A]~]: ~%
                    ~5@T~A.~%"
            name parent-description description)))
