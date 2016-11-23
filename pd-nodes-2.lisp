(defpackage :pd-nodes/2
  (:use :common-lisp
        :pd-structs)
  (:import-from :pd-writer
                :connect
                :add-node)
  (:export :msg))

(in-package :pd-nodes/2)

;; --------------------------------------------------------------------------------
;; msg node
;; 
(defun replace/2 (old new sequence)
  (with-output-to-string (out)
    (labels ((fn (old new sequence)
               (let ((found (search old sequence)))
                 (if found
                     (progn 
                       (format nil "found is ~a" found)
                       (princ (subseq sequence 0 found) out)
                       (princ new out)
                       (princ (fn old new
                                  (subseq sequence 
                                          (+ found
                                             (length old))))))
                     (princ sequence out)))))
      (fn old new sequence))
    out))

(defun escape-msg-args (args)
  (mapcar #'(lambda (arg)
              (if (stringp arg)
                  (replace/2 "$" "\\$" (replace/2 ";" "\\;" arg))
                  arg))
          args))

(defun msg (&rest args)
  (let ((n (make-instance 'msg-node)))
    (connect n (escape-msg-args args))
    (add-node n)
    n))

