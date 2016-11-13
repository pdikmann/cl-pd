(defpackage :pd-writer
  (:use :common-lisp))

(in-package :pd-writer)

(defparameter *patch* "")
(defparameter *nodes* nil)
(defparameter *connections* nil)

(defstruct node
  (name)                  ; string representation of object
  (init-args)             ; string representation of initial arguments
  (id (gensym))       ; unique symbol (in lisp)
  (index)                 ; unique number (in patch)
  )

(defstruct connection
  (out-id)                          ; gensym of source node
  (out-port)                           ; index of output port
  (in-id)                           ; gensym of target node
  (in-port)                            ; index of input port
  )

(defun write-node (n)
  (princ (concatenate 'string
                      "#X obj 0 0 "
                      (node-name n) " "
                      (node-init-args n)
                      ";"
                      (string #\newline))
         nil))

(defun to-string (num)
  (format nil "~d" num))

(defun write-connection (c)
  (let ((out (find (connection-out-id c) *nodes* :key #'node-id))
        (in (find (connection-in-id c) *nodes* :key #'node-id)))
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

;; --------------------------------------------------------------------------------
;; nodes
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

(defun +~ (&optional a b)
  (let ((n (make-node :name "+~")))
    (mapcar (lambda (arg i)
              (if (node-p arg)
                  ;; connect node
                  (add-connection (node-id arg) 0
                                  (node-id n) i)
                  ;; add literal to init-args
                  (setf (node-init-args n)
                        (concatenate 'string
                                     (node-init-args n) " "
                                     (to-string arg)))))
            (list a b) ; args
            (list 0 1) ; input ports
            )
    (add-node n)
    n))

(defun dac~ () nil)

;; --------------------------------------------------------------------------------
;; usage
;;
(with-patch "test.pd"
  ;; (let ((out (+~ (osc~ 100)
  ;;                (osc~ 200))))
  ;;   (dac~ out
  ;;         out))
  (+~ 789 (osc~ (osc~ 123)))
  (+~ 123)
  )
