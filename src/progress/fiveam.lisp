;;;; fiveam.lisp --- Progress adapter for the fiveam framework.
;;;;
;;;; Copyright (C) 2013, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.DE>

(cl:in-package #:test.result.progress)

(defvar *suite-progress?* nil)

(defmethod 5am::%run :around ((test-spec 5am::test-suite))
  (flet ((do-it ()
           (let ((5am:*test-dribble* (make-broadcast-stream)) ; TODO avoid re-creating on recursive calls
                 (*suite-progress?* (gensym "CHILDREN")))
             (more-conditions:with-sequence-progress (*suite-progress?* (alexandria:hash-table-values (5am::tests test-spec)))
               (call-next-method)))))
    (if *suite-progress?*
        (progn
          (more-conditions:progress *suite-progress?* nil "~A" (5am::name test-spec))
          (do-it))
        (more-conditions:with-trivial-progress (:suite "~A" (5am::name test-spec))
          (do-it)))))

(defmethod 5am::run-resolving-dependencies :around ((test 5am::test-case))
  (let ((dependencies (5am::depends-on test)))
    (if (or (not (typep dependencies 'sequence))
            (alexandria:emptyp dependencies))
        (call-next-method)
        (let ((*suite-progress?* nil)
              (dependencies (gensym "DEPENDENCIES")))
          (more-conditions:with-trivial-progress (dependencies)
            (more-conditions:progress dependencies .5)
            (call-next-method))))))

(defmethod 5am::%run :around ((test-spec 5am::test-case))
  (when *suite-progress?*
    (more-conditions:progress *suite-progress?* nil "~A" (5am::name test-spec)))
  (let ((*suite-progress?* nil))
    (call-next-method)))
