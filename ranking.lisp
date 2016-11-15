(defpackage :pd-ranking
  (:use :common-lisp
        :pd-structs)
  (:import-from :pd-structs
                :node-name
                :node-init-args
                :node-id
                :node-index
                :node-rank
                :node-x
                :node-y)
  (:export rank))

(in-package :pd-ranking)

(defparameter +x-spacing+ 100)
(defparameter +y-spacing+ 50)

(defun make-nodes-hash (nodes)
  "turn list of nodes into hashtable for easier processing"
  (let ((h (make-hash-table)))
    (mapcar (lambda (n)
              (setf (gethash (node-id n) h) n))
            nodes)
    h))

(defun rank-hash (nodes-hash connections)
  "rank nodes based on connections. 
   tries putting the receiving (target) node below the originating (source) node."
  (if (null connections)
      nodes-hash
      (let* ((c (first connections))
             (n1 (gethash (connection-out-id c) nodes-hash))
             (n2 (gethash (connection-in-id c) nodes-hash)))
        (cond
          ;; both node-rank null: set both
          ((and (null (node-rank n1))
                (null (node-rank n2)))
           (setf (node-rank n1) 0)
           (setf (node-rank n2) 1))
          ;; target node not ranked: put target node below source
          ((null (node-rank n2))
           (setf (node-rank n2)
                 (1+ (node-rank n1))))
          ;; source node not ranked: put source node above source
          ((null (node-rank n1))
           (setf (node-rank n1)
                 (1- (node-rank n2)))))
        ;; update hash-table
        (setf (gethash (connection-out-id c) nodes-hash) n1)
        (setf (gethash (connection-in-id c) nodes-hash) n2)
        ;; recur
        (rank-hash nodes-hash (rest connections)))))

(defun min-rank (nodes-hash)
  "determine smallest node-rank in nodes-hash"
  (let ((n most-positive-fixnum))
    (maphash (lambda (k v)
               (when (< (node-rank v) n)
                 (setf n (node-rank v))))
             nodes-hash)
    n))

(defun y-align (nodes-hash)
  "set node-y position based on node-rank"
  (let ((min (min-rank nodes-hash)))
    (maphash (lambda (k v)
               (setf (node-y (gethash k nodes-hash))
                     (* +y-spacing+
                        (+ (node-rank v)
                           (* -1 min)))))
             nodes-hash)
    nodes-hash))

(defun x-align (nodes-hash)
  "set node-x position based on number of nodes on same node-y position"
  (let ((stacks (make-hash-table)))
    (maphash (lambda (k v)
               ;; update stack size for current rank
               (if (gethash (node-rank v) stacks)
                   (setf (gethash (node-rank v) stacks)
                         (1+ (gethash (node-rank v)  stacks)))
                   (setf (gethash (node-rank v) stacks)
                         0))
               ;; position right of stack
               (setf (node-x (gethash k nodes-hash))
                     (* +x-spacing+
                        (gethash (node-rank v) stacks))))
             nodes-hash)
    nodes-hash))

(defun rank (nodes connections)
  "rank, then position, given list of nodes"
  (let ((nodes-hash
         (x-align
          (y-align
           (rank-hash
            (make-nodes-hash nodes)
            connections)))))
    ;; return nodes in original ordering
    (mapcar (lambda (original)
              (gethash (node-id original) nodes-hash))
            nodes)))
