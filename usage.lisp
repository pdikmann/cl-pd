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

(pdx:with-patch ("cascade.pd")
  (let* ((freqs '(100 200 400 800 1600 3200 6400))
         (out (pd::*~ (/ 1.0 (length freqs)) (cascade (reverse freqs)))))
    (pd::dac~ out out)))

(pdx:with-patch ("cascade2.pd")
  (let* ((freqs '(100 120 130 135 137))
         (out (pd::*~ (/ 1.0 (length freqs)) (cascade (reverse freqs)))))
    (pd::dac~ out out)))


(pdx::with-patch ("color-range.pd" :width 1040 :height 128)
  (mapcar (lambda (r x)
            (pd::bng :x (* x 16) :y 0
                     :bg-color (pdx::color/file r 0 0))
            (pd::bng :x (* x 16) :y 16
                     :bg-color (pdx::color/file 0 r 0))
            (pd::bng :x (* x 16) :y 32
                     :bg-color (pdx::color/file 0 0 r))
            (pd::bng :x (* x 16) :y 48
                     :bg-color (pdx::color/file r r 0))
            (pd::bng :x (* x 16) :y 64
                     :bg-color (pdx::color/file r 0 r))
            (pd::bng :x (* x 16) :y 80
                     :bg-color (pdx::color/file 0 r r))
            (pd::bng :x (* x 16) :y 96
                     :bg-color (pdx::color/file r r r)))
          (loop
             for r from 0 to 255 by 4
             collect r)
          (loop
             for x from 0 to 63
             collect x)))

(pdx:with-patch ("test.pd" :graph-on-parent t
                                          :view-width 300
                                          :view-height 100
                                          ;; :hide-object-name t
                                          )
  (let* ((my-adder (pd::+ 1))
         (my-flt (pd::float 0
                            (pd::metro 500
                                       (pd::loadbang))
                            my-adder)))
    (pdx:connect my-adder my-flt)
    (pd::symbol "$1 $2 $3")
    (pd::print "yeah"
               (pd::msg "message says hello $1" my-flt))
    (pd::outlet my-flt)
    (pd::cnv :x 0 :y 0 :width 500)      ; canvas
    (pd::+ :x 100 :y 0 123 456)         ; manual positioning
    (pd::print "junk food"
               (pd::bng :x 50 :y 50 :size 32
                        :interrupt 5 :hold 250))
    ))
