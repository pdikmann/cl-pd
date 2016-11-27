(defpackage :pd-nodes/4
  (:use :common-lisp
        :pd-structs)
  (:import-from :pd-writer
                :connect
                :add-node)
  (:export :node-template))

(in-package :pd-nodes/4)

(defmacro node-template (type name)
  "defines a function #'NAME that will create a node of type TYPE when called"
  `(defun ,name (&rest args)
     (multiple-value-bind (keys args)
         ;; check for keyword-arguments AT BEGINNING of argument list
         (labels ((get-keys (args &optional (keys nil))
                    (cond
                      ((keywordp (first args))
                       (get-keys (cddr args)
                                 (append keys (ldiff args (cddr args)))))
                      (t
                       (values keys args)))))
           (get-keys args))
       ;; create node and connect it
       (let ((n (apply #'make-instance
                       ,type
                       :name (format nil "~(~a~)" ',name)
                       keys)))
         (apply #'connect n args)
         (add-node n)
         n))))
