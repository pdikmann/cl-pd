(defpackage :pd-structs
  (:use :common-lisp)
  (:export :write-node
           :to-string
           ;; --------------------------------
           :node
           :object-node
           :self-node
           :patch-node
           :ui-node
           :bng-node
           :tgl-node
           :cnv-node
           :vsl-node
           :hsl-node
           ;; from node
           :node-p
           :node-name
           :node-init-args
           :node-id
           :node-index
           :node-rank
           :node-x
           :node-y
           ;; from ui-node
           :node-send-symbol   
           :node-receive-symbol
           :node-label-text    
           :node-label-x       
           :node-label-y       
           :node-font-family   
           :node-font-size     
           :node-bg-color      
           :node-fg-color      
           :node-label-color   
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

;; --------------------------------------------------------------------------------
;; node classes
;; 
(defclass node ()
  ((id
    :accessor node-id
    :initform (gensym))
   (name
    :accessor node-name
    :initarg :name)
   (index
    :accessor node-index)
   (x
    :initarg :x
    :initform nil
    :accessor node-x)
   (y
    :initarg :y
    :initform nil
    :accessor node-y)
   (rank
    :initform nil
    :accessor node-rank))
  )

(defgeneric node-p (n) (:documentation "test wether a thing is a node (or one of its subclasses)"))
(defmethod node-p (n) nil)
(defmethod node-p ((n node)) t)

(defclass object-node (node)
  ((init-args
    :accessor node-init-args
    :initarg :init-args
    :initform ""
    )))

(defclass self-node (object-node)
  ;; for nodes like "msg" and "text" that are represented as "#X msg" or "#X text"
  ;; (e.g. representing themselves)
  ())

(defclass patch-node (object-node) ())

(defclass ui-node (node)
  ((send-symbol    :accessor node-send-symbol    :initarg :send        :initform "empty")
   (receive-symbol :accessor node-receive-symbol :initarg :receive     :initform "empty")
   (label-text     :accessor node-label-text     :initarg :label       :initform "empty")
   (label-x        :accessor node-label-x        :initarg :label-x     :initform 17)
   (label-y        :accessor node-label-y        :initarg :label-y     :initform 7)
   (font-family    :accessor node-font-family    :initarg :font        :initform 0)
   (font-size      :accessor node-font-size      :initarg :font-size   :initform 10)
   (bg-color       :accessor node-bg-color       :initarg :bg-color    :initform -262144)
   (fg-color       :accessor node-fg-color       :initarg :fg-color    :initform -1)
   (label-color    :accessor node-label-color    :initarg :label-color :initform -1)))

(defclass bng-node (ui-node)
  ((size      :initarg :size      :initform 15)
   (hold      :initarg :hold      :initform 250)
   (interrupt :initarg :interrupt :initform 50)
   (init      :initarg :init      :initform 0)))

(defclass tgl-node (ui-node)
  ((size          :initarg :size          :initform 15)
   (init          :initarg :init          :initform 0)
   (init-value    :initarg :init-value    :initform 0)
   (nonzero-value :initarg :nonzero-value :initform 1)))

(defclass cnv-node (ui-node)
  ((bg-color    :initarg :bg-color    :initform -233017) ; different default color
   (label-color :initarg :label-color :initform -66577) ; different default color
   (size        :initarg :size        :initform 15)
   (width       :initarg :width       :initform 100)
   (height      :initarg :height      :initform 60)
   (unknown :initform 0)))

(defclass slider-node (ui-node)
  ((width           :initarg :width           :initform nil)
   (height          :initarg :height          :initform nil)
   (bottom          :initarg :bottom          :initform 0)
   (top             :initarg :top             :initform 127)
   (log             :initarg :log             :initform 0)
   (init            :initarg :init            :initform 0)
   (default         :initarg :default         :initform 0) ; default means: saved slider position, in pixels.
   (steady-on-click :initarg :steady-on-click :initform 1)))

(defclass vsl-node (slider-node)
  ((width :initform 15)
   (height :initform 127)))

(defclass hsl-node (slider-node)
  ((width :initform 127)
   (height :initform 15)))

;; --------------------------------------------------------------------------------
;; templating
;; 
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

(defmethod write-node ((n self-node))
  (fill-template '("#X" name x y init-args) n))

(defmethod write-node ((n patch-node))
  (fill-template '("#X obj" x y init-args) n))

(defmethod write-node ((n bng-node))
  (fill-template '("#X obj" x y
                   name                 ; "bng"
                   size
                   hold interrupt
                   init
                   send-symbol receive-symbol
                   label-text
                   label-x label-y
                   font-family font-size
                   bg-color fg-color label-color)
                 n))

(defmethod write-node ((n tgl-node))
  (fill-template '("#X obj" x y
                   name                 ; "tgl"
                   size
                   init
                   send-symbol receive-symbol
                   label-text
                   label-x label-y
                   font-family font-size
                   bg-color fg-color label-color
                   init-value nonzero-value)
                 n))

(defmethod write-node ((n cnv-node))
  (fill-template '("#X obj" x y
                   name                 ; "cnv"
                   size
                   width height
                   send-symbol receive-symbol
                   label-text
                   label-x label-y
                   font-family font-size
                   bg-color label-color
                   unknown)
                 n))

(defmethod write-node ((n slider-node))
  (fill-template '("#X obj" x y
                   name                 ; "hsl" or "vsl"
                   width height
                   bottom top
                   log
                   init
                   send-symbol
                   receive-symbol
                   label-text
                   label-x label-y
                   font-family font-size
                   bg-color fg-color label-color
                   default steady-on-click
                   )
                 n))

;; --------------------------------------------------------------------------------
;; misc
;; 
(defstruct connection
  (out-id)                              ; gensym of source node
  (out-port)                            ; index of output port
  (in-id)                               ; gensym of target node
  (in-port)                             ; index of input port
  )

(defstruct port
  (number)
  (node))
