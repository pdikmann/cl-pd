(defpackage :pd-nodes/3
  (:use :common-lisp
        :pd-structs)
  (:export :bng))

(in-package :pd-nodes/3)

(defun bng (&rest args) t
  ;; (let ((n (apply #'make-instance
  ;;                 'bng-node
  ;;                 args)))
  ;;   (connect n ))
  )
