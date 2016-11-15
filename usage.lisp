(defpackage :pd-usage
  (:use :common-lisp
        ;;:pd-writer ; aka pdx
        ;;:pd-objects ; aka pd
        ))

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
;; - to connect to a specific input port, you have to put the connectee
;;   into the corresponding argument position,
;;   e.g. to connect to the second port while skipping the first:
;;   
;;   (+~ nil (osc~ 100))
;;   
;;   and with initialization arguments:
;;   
;;   (+~ 10 20 nil (number 20))
;;
;; - more complex objects use keyword parameters, e.g.
;;
;;  (hsl :width 100 :height 15 ...)

(load "pd-init.lisp")

(defun detuned (freq)
  (pd::osc~
   (pd::+~
    freq
    (pd::*~
     (/ freq 20.0)
     (pd::osc~ (random .2))))))

(defun cascade (lst)
  (if (null (rest lst))
      (detuned (first lst))
      (pd::+~ (pd::*~ (* (/ 1.0 (length lst))
                         (/ 1.0 (length lst)))
                      (detuned (first lst)))
              (cascade (rest lst)))))

(pdx:with-patch "~/pd/pd-writer/cascade.pd"
  (let* ((freqs '(100 200 400 800 1600 3200 6400))
         (out (pd::*~ (/ 1.0 (length freqs)) (cascade (reverse freqs)))))
    (pd::dac~ out out)))

(pdx:with-patch "~/pd/pd-writer/cascade2.pd"
  (let* ((freqs '(100 120 130 135 137))
         (out (pd::*~ (/ 1.0 (length freqs)) (cascade (reverse freqs)))))
    (pd::dac~ out out)))

(pdx:with-patch "~/pd/pd-writer/test.pd"
  (let* ((my-adder (pd::+ 1))
         (my-flt (pd::float 0
                            (pd::metro 500
                                       (pd::loadbang))
                            my-adder)))
    (pdx:connect my-adder (list my-flt)) ;; make this more beautiful please
    (pd::print "yeah" my-flt)
    (pd::outlet my-flt)))
