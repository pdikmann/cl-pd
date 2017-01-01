(defpackage :pd-nodes/4
  (:use :common-lisp
        :pd-structs)
  (:import-from :pd-writer
                :connect
                :add-node)
  (:export :node-template))

(in-package :pd-nodes/4)

(defun replace/2 (old new sequence)
  (with-output-to-string (out)
    (labels ((fn (old new sequence)
               (let ((found (search old sequence)))
                 (if found
                     (progn 
                       ;(format nil "found is ~a" found)
                       (princ (subseq sequence 0 found) out)
                       (princ new out)
                       (fn old new
                           (subseq sequence 
                                   (+ found
                                      (length old)))))
                     (princ sequence out)))))
      (fn old new sequence))
    out))

(defun escape-string-args (args)
  (mapcar #'(lambda (arg)
              (if (stringp arg)
                  (replace/2 "$" "\\$" (replace/2 ";" "\\;" arg))
                  arg))
          args))

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
         (apply #'connect n (escape-string-args args))
         (add-node n)
         n))))
