(load "main.lisp")
(load "objects.lisp")

(defpackage :pd-usage
  (:use :common-lisp
        :pd-writer
        :pd-objects))

(in-package :pd-usage)

;; --------------------------------------------------------------------------------
;; usage
;;

;; rules:
;; 
;; - literals (strings, numbers, symbols) are concatenated verbatim to the object literal.
;;   (this is how pd initializes objects with arguments)
;; 
;; - anything else (nil, nodes, ports) is connected to the input ports of the object.
;; 
;; - to connect to a specific input port, you have to put the connectee into the corresponding argument position, e.g. to connect to the second port while skipping the first:
;;   (+~ nil (osc~ 100))
;;   and with initialization arguments:
;;   (+ 10 20 nil (number 20))

(with-patch "~/pd/pd-writer/test.pd"
  ;; (let ((out (+~ (osc~ 100)
  ;;                (osc~ 200))))
  ;;   (dac~ out
  ;;         out))

  ;; (dac~ (+~ 100 nil (port 1 (trigger 'b 'b 'b)))
  ;;       (+~ 789 (osc~ (osc~ 123))))
  ;; (+~ 123)

  (pd:< (pd:float 100)
        (pd:float 200 (pd:bang)))
  )


;; (with-patch "test.pd"
;;   (let ((out (pd +~
;;                  (pd osc~ 100)
;;                  (pd osc~ 200))))
;;     (pd dac~
;;         out
;;         out)))
