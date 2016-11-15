(defpackage :pd-structs
  (:use :common-lisp)
  (:export :write-node
           :node-p
           :to-string
           ;; --------------------------------
           :node
           :object-node
           ;; :node
           ;; :make-node
           :node-name
           :node-init-args
           :node-id
           :node-index
           :node-rank
           :node-x
           :node-y
           ;; --------------------------------
           :connection
           :make-connection
           :connection-p
           :connection-out-id
           :connection-out-port
           :connection-in-id
           :connection-in-port
           ;; --------------------------------
           :port
           :make-port
           :port-p
           :port-number
           :port-node))

(in-package :pd-structs)

;; idea: use types (object, message, floatatom, symbolatom) for different node templates
;; (maybe objects that all implement a 'template'-message?)

(defclass node ()
  ((id
    :accessor node-id
    :initform (gensym))
   (index :accessor node-index)
   (x :accessor node-x)
   (y :accessor node-y)
   (rank
    :initform nil
    :accessor node-rank))
  )

(defgeneric node-p (n) (:documentation "test wether a thing is a node (or one of its subclasses)"))
(defmethod node-p (n) nil)
(defmethod node-p ((n node)) t)

(defclass object-node (node)
  ((name
    :accessor node-name
    :initarg :name
    )
   (init-args
    :accessor node-init-args
    :initarg :init-args
    :initform ""
    )))

(defun to-string (any)
  (format nil "~a" any))

(defun fill-template (template n &optional s)
  "fill provided template with node attributes"
  (if (null template)
      (concatenate 'string s ";" (string #\newline))
      (let ((arg (first template)))
        (fill-template (rest template)
                       n
                       (concatenate 'string
                                    s
                                    " "
                                    (to-string
                                     (if (symbolp arg)
                                         (slot-value n arg)
                                         arg)))))))

(defgeneric write-node (n)
  (:documentation "create .pd-compliant string representation of node objects"))

(defmethod write-node ((n object-node))
  (fill-template '("#X obj" x y name init-args) n))

;; (defstruct node
;;   name                               ; string representation of object
;;   init-args ; string representation of initial arguments (only object-nodes)
;;   (id (gensym))              ; unique symbol (identifier in lisp)
;;   index                      ; unique number (occurence in patch file)
;;   rank
;;   x y                                   ; position
;;   )

(defstruct connection
  (out-id)                              ; gensym of source node
  (out-port)                            ; index of output port
  (in-id)                               ; gensym of target node
  (in-port)                             ; index of input port
  )

(defstruct port
  (number)
  (node))
