(defpackage :pd-writer
  (:nicknames :pdx)
  (:use :common-lisp
        :pd-structs
        :pd-ranking)
  (:export :with-patch
           :port
           :connect
           :color/live
           :color/file))

(in-package :pd-writer)

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

(defun write-patch (&key
                      (width 512)
                      (height 512)
                      graph-on-parent
                      view-width
                      view-height
                      hide-object-name)
  (with-open-file (f *patch* :direction :output :if-exists :supersede)
    (format f "#N canvas 0 0 ~d ~d 10;~%" width height) ; patch init
    ;; ... nodes
    (mapcar (lambda (n)
              (princ (write-node n) f))
            (reverse (rank *nodes*
                           *connections*
                           view-height)))
    ;; ... connections
    (mapcar (lambda (c)
              (princ (write-connection c) f))
            *connections*)
    ;; graph-on-parent
    (when graph-on-parent
      (format f "#X coords 0 -1 1 1 ~d ~d ~d 0 0;~%"
              view-width
              view-height
              (if hide-object-name 2 1)))
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
(defmacro with-patch (patch-config &rest form)
  `(progn
    (setq *patch* ,(first patch-config))
    (setq *nodes* nil)
    (setq *connections* nil)
    ,@form
    (write-patch ,@(rest patch-config))))

(defun port (number node) ; TODO rename to "outlet", flip argument order
  (make-port :number number
             :node node))

(defun connect (n &rest args)
  (labels
      ((recur (n port args &optional (inc-port t))
         (if
          (not (null args))
          (let ((arg (first args))
                (remaining (rest args)))
            (cond
              ;; connect node
              ((node-p arg)      
               (add-connection (node-id arg) 0   ; default output port
                               (node-id n) port) ; argument position
               (recur n (if inc-port (1+ port) port)
                      remaining inc-port))
              ;; connect node (specific output port)
              ((port-p arg)
               (add-connection (node-id (port-node arg)) (port-number arg)
                               (node-id n) port)
               (recur n (if inc-port (1+ port) port)
                      remaining inc-port))
              ;; all elements of a list are connected to the same inlet
              ((listp arg)
               (recur n port arg nil)
               (recur n (if inc-port (1+ port) port)
                      remaining inc-port))
              ;; literals are added as init-args
              ((not (null arg))     
               (setf (node-init-args n)
                     (concatenate 'string
                                  (node-init-args n) " "
                                  (to-string arg)))
               (recur n port remaining inc-port))
              ;; an argument of `nil` explicitly increments the input port counter
              ((null arg)
               (recur n (if inc-port (1+ port) port)
                      remaining inc-port))))
          n)))
    (recur n 0 args t)))

(defun color/live (r g b)
  "used to assign colors at runtime, e.g. when adressing ui-nodes by pd messages"
  (1- (+ (* r -65536)
         (* g -256)
         (* b -1))))

(defun color/file (r g b)
  "used to assign colors at compile-time, e.g. when writing them into a patch file"
  (destructuring-bind (r2 g2 b2)
      (mapcar (lambda (x) (floor (/ x 4)))
              (list r g b))
    (1- (+ (* r2 -4096)
           (* g2 -64)
           (* b2 -1)))))
