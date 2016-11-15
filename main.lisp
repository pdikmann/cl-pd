(defpackage :pd-writer
  (:nicknames :pdx)
  (:use :common-lisp
        :pd-structs
        :pd-ranking)
  (:import-from :pd-structs
                :node-name
                :node-init-args
                :node-id
                :node-index
                :node-rank
                :node-x
                :node-y)
  (:export :with-patch
           :port
           :connect))

(in-package :pd-writer)
;;(unintern :node)

;; --------------------------------------------------------------------------------
;; data
;; 
(defparameter *patch* "")
(defparameter *nodes* nil)
(defparameter *connections* nil)

;; --------------------------------------------------------------------------------
;; pd
;; 

(defun write-connection (c)
  (let ((out (find (connection-out-id c) *nodes* :key #'node-id))
        (in (find (connection-in-id c) *nodes* :key #'node-id)))
    (cond ((or (null out)
               (null in))
           (error "wow, no node found!")))
    (concatenate 'string
                 "#X connect "
                 (to-string (node-index out)) " "
                 (to-string (connection-out-port c)) " "
                 (to-string (node-index in)) " "
                 (to-string (connection-in-port c))
                 ";"
                 (string #\newline))))

(defun write-patch ()
  (with-open-file (f *patch* :direction :output :if-exists :supersede)
    (format f "#N canvas 0 0 512 512 10;~%") ; patch init
    ;; ... nodes
    (mapcar (lambda (n)
              (princ (write-node n) f))
            (reverse (rank *nodes* *connections*)))
    ;; ... connections
    (mapcar (lambda (c)
              (princ (write-connection c) f))
            *connections*)
    t))

(defun add-connection (out-id out-port in-id in-port)
  (let ((c (make-connection :out-id out-id
                            :out-port out-port
                            :in-id in-id
                            :in-port in-port)))
    (push c *connections*)))

(defun add-node (n)
  ;; this could incorporate the duplication between node definitions
  (setf (node-index n) (length *nodes*))
  (push n *nodes*))

;; --------------------------------------------------------------------------------
;; user-facing
;; 
(defmacro with-patch (patch-name &rest form)
  `(progn
    (setq *patch* ,patch-name)
    (setq *nodes* nil)
    (setq *connections* nil)
    ,@form
    (write-patch)))

(defun port (number node)
  (make-port :number number
             :node node))

(defun connect (n args)
  (let ((port 0))
    (mapcar (lambda (arg)
              (cond
                ((node-p arg)                    ; connect node
                 (add-connection (node-id arg) 0 ; default output port
                                 (node-id n) port) ; argument position
                 (incf port))
                ((port-p arg)
                 (add-connection (node-id (port-node arg)) (port-number arg)
                                 (node-id n) port)
                 (incf port))
                ((not (null arg))       ; add init-arg literal
                 (setf (node-init-args n)
                       (concatenate 'string
                                    (node-init-args n) " "
                                    (to-string arg))))
                ((null arg)
                 (incf port)            ; an argument of `nil` explicitly increments the port counter
                 )))
            args))
  n)
