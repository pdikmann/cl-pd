(defpackage :pd-structs
  (:use :common-lisp)
  (:export :node
           :make-node
           :node-p
           :node-name
           :node-init-args
           :node-id
           :node-index
           :node-rank
           :node-x
           :node-y
           :connection
           :make-connection
           :connection-p
           :connection-out-id
           :connection-out-port
           :connection-in-id
           :connection-in-port
           :port
           :make-port
           :port-p
           :port-number
           :port-node))

(in-package :pd-structs)

;; idea: use types (object, message, floatatom, symbolatom) for different node templates
;; (maybe objects that all implement a 'template'-message?)

(defstruct node
  name                               ; string representation of object
  init-args ; string representation of initial arguments (only object-nodes)
  (id (gensym))              ; unique symbol (identifier in lisp)
  index                      ; unique number (occurence in patch file)
  rank
  x y                                   ; position
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
