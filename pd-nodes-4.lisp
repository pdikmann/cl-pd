(defpackage :pd-nodes/4
  (:use :common-lisp)
  (:import-from :pd-structs
                :node
                :object-node)
  (:import-from :pd-writer
                :connect
                :add-node)
  (:export :object-node-template))

(in-package :pd-nodes/4)

(defmacro object-node-template (name)
  `(defun ,name (&rest args)
     (multiple-value-bind (keys args) ;; check for keyword-arguments AT BEGINNING of argument list
         (labels ((get-keys (args &optional (keys nil))
                    (cond
                      ((keywordp (first args))
                       (get-keys (cddr args)
                                 (append keys (ldiff args (cddr args)))))
                      (t
                       (values keys args)))))
           (get-keys args))
       (let ((n (apply #'make-instance
                       'object-node
                       :name (format nil "~(~a~)" ',name)
                       keys)))
         (connect n args)
         (add-node n)
         n))))
