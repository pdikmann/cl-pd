(defpackage :pd-writer
  (:use :common-lisp)
  (:export :with-patch
           :port))

(in-package :pd-writer)

(defparameter *patch* "")
(defparameter *nodes* nil)
(defparameter *connections* nil)

;; --------------------------------------------------------------------------------
;; data
;; 

;; idea: use types (object, message, floatatom, symbolatom) for different node templates
;; (maybe objects that all implement a 'template'-message?)

(defstruct node
  (name)                  ; string representation of object
  (init-args)             ; string representation of initial arguments (only object-nodes)
  (id (gensym))           ; unique symbol (identifier in lisp)
  (index)                 ; unique number (occurence in patch file)
  )

(defstruct connection
  (out-id)                              ; gensym of source node
  (out-port)                            ; index of output port
  (in-id)                               ; gensym of target node
  (in-port)                             ; index of input port
  )

(defstruct port
  (number)
  (node))

;; --------------------------------------------------------------------------------
;; pd
;; 
(defun to-string (any)
  (format nil "~a" any))

;; idea: on the templating level this is not so cool – the node-struct would have to cover all fields of all objects ... :(

;; (defun object-node-template (n)
;;   (concatenate 'string
;;                "#X obj 0 0 "
;;                (node-name n) " "
;;                (node-init-args n)
;;                ";"
;;                (string #\newline)))

;; (defun message-node-template (n)
;;   (concatenate 'string
;;                "#X msg 0 0 "
;;                (node-init-args n)
;;                ";"
;;                (string #\newline)))

(defun write-node (n)
  (concatenate 'string
               "#X obj 0 0 "
               (node-name n) " "
               (node-init-args n)
               ";"
               (string #\newline)))

(defun write-connection (c)
  (let ((out (find (connection-out-id c) *nodes* :key #'node-id))
        (in (find (connection-in-id c) *nodes* :key #'node-id)))
    (cond ((or (null out)
               (null in))
           (error "wow, no node found!")))
    (princ (concatenate 'string
                        "#X connect "
                        (to-string (node-index out)) " "
                        (to-string (connection-out-port c)) " "
                        (to-string (node-index in)) " "
                        (to-string (connection-in-port c))
                        ";"
                        (string #\newline))
           nil)))

(defun write-patch ()
    (with-open-file (f *patch* :direction :output :if-exists :supersede)
      (format f "#N canvas 0 0 512 512 10;~%") ; patch init
      ;; ... nodes
      (mapcar (lambda (n) (princ (write-node n) f))
              (reverse *nodes*))
      ;; ... connections
      (mapcar (lambda (c) (princ (write-connection c) f))
              *connections*)
      ))


(defun process-node-args (n args)
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

;; --------------------------------------------------------------------------------
;; user-facing
;; 
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

;; --------------------------------------------------------------------------------
;; node library
;;
(defun osc~ (freq)
  (let ((n (make-node :name "osc~")))
    (if (node-p freq)
        (add-connection (node-id freq) 0 ; maybe a wrapper object that you can ask?
                        (node-id n) 0)
        (setf (node-init-args n)
              (to-string freq))   ; assume number
        )
    (add-node n)
    n))

(defun +~ (&rest args)
  (let ((n (make-node :name "+~")))
    ;; improve: init literals and connections should use independent port indices
    (process-node-args n args)
    (add-node n)
    n))

(defun dac~ (&rest args)
  (let ((n (make-node :name "dac~")))
    (process-node-args n args)
    (add-node n)
    n))

(defun trigger (&rest stuff)
  (let ((n (make-node :name "t")))
    (process-node-args n stuff)
    (add-node n)
    n))
