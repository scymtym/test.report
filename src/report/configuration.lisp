;;;; configuration.lisp --- Configuration of reporting styles.
;;;;
;;;; Copyright (C) 2013, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:test.report)

(configuration.options:define-schema *default-schema*
  ("verbose-failures" :type 'boolean :default t))

(configuration.options:define-schema *schema*
  ("report"
   (:wild
    ("style" :type '(member))
    ("style"
     ("default" *default-schema*))
    ("target" :type '(or (member :standard-output) pathname))
    ("enabled" :type 'boolean :default t)))
  ("reports" :type '(list string :inherit? t)))

(setf (options:find-child "report.*.style.default" *schema*) *default-schema*)

(defun update-schema ()
  (let ((item (options:find-option "report.*.style" *schema*)))
    (reinitialize-instance
     item
     :type `(member ,@(mapcar #'service-provider:service-name
                              (service-provider:service-providers 'style))))))

(eval-when (:load-toplevel :execute)
  (update-schema))

(let* ((c (options:make-configuration *schema*))
       (ss (options.sources:make-source :stream
                                        :stream (make-string-input-stream "
reports = foo

[report.foo]
enabled = true
target = STANDARD-OUTPUT
style = DEFAULT
style.default.verbose-failures = false")
                                        :syntax :ini))
       (s (make-instance 'options:standard-synchronizer :target c)))
  (options.sources:initialize ss *schema*)
  (options.sources:process ss s)

  (dolist (report (options:option-value (options:find-option "reports" c)))
    (when (options:option-value (options:find-option (list "report" report "enabled") c))
      (let ((style (options:option-value (options:find-option (list "report" report "style") c))))
            (report (test.report::handle-lift-result cl-user::*result*)
                (list* style (mappend (lambda (option)
                                        (list (make-keyword
                                               (string-upcase
                                                (lastcar (options:name-components
                                                          (options:option-name option)))))
                                              (options:option-value option)))
                                     (options:find-options (list "report" report "style" (string-downcase style) :wild) c)))
               (let ((target (options:option-value (options:find-option (list "report" report "target") c))))
                 (case target
                   (:standard-output *standard-output*)
                   (t                target)))))))

  (list c (options::sub-configuration (options:make-name "report.foo.**") c)))
