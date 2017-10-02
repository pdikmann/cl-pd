(defpackage :pd-usage
  (:use :common-lisp
        ;;:pd-writer ; aka pdx
        ;;:pd-objects ; aka pd
        ))

(in-package :pd-usage)

(load "pd-init.lisp") ;; FIXME use asdf or similar

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

;; 1) basic usage
(pdx:with-patch             ; with-patch writes a file.
    ("basic-usage.pd")      ; these are options, e.g. file name.
  (pd::text "hello world")) ; all nodes are accessible in the pd package.
                            ; (use of the "::"-qualifier is a workaround because many PureData names would shadow core CL functions (e.g. pd::list).)

;; 2) auto layouter
(pdx:with-patch
    ("auto-layouter.pd"
     :width 200
     :height 200)
  (pd::print "result"
             (pd::+ (pd::msg 1)
                    (pd::msg 2)))) ; connections are laid out by a primitive auto-layouter (so you can still figure out what's going on).

;; 3) node arguments
(pdx:with-patch
    ("node-arguments.pd")
  (pd::route 1 2)                             ; numbers and strings are used as literal arguments, just like creating a node in pd.
  (pd::msg "hello my name is $0")             ; special characters like $ and ; work as expected when used in strings.
  (pd::+ (pd::msg 1) (pd::msg 2))             ; other nodes are connected to the inlets in the order of the arguments, e.g. the leftmost argument is connected to the leftmost inlet.
  (pd::- nil (pd::msg 3))                     ; to skip an inlet, put nil as the argument.
  (pd::print "hello" (list (pd::msg "bob")
                           (pd::msg "mandy")
                           (pd::msg "ada")))  ; to connect multiple nodes to the same outlet, simply pass in a list.
  ;;
  (pd::text :x 10 :y 10
            "manually placed")                ; there's also keyword arguments!
                                              ; keyword arguments need to go in front of literals and nodes.
                                              ; check which arguments are available (as :initarg in the defclass-forms) in pd-structs.lisp.
                                              ; all nodes support :x and :y.
  ;;
  (let ((trig (pd::t "b f" (pd::msg "123")))) ; what about multiple outlets?
    (pd::print "float" (pdx:port 1 trig))     ; to connect to a specific outlet, use pdx:port.
    (pd::print "bang" trig))                  ; (outlet 0 (the leftmost one) is the default.)
  ;;
  (let ((bob (pd::/ 10))
        (mandy (pd::==)))
    (pdx:connect bob (pd::msg 3))             ; you can always add more connections by invoking pdx:connect.
    (pdx:connect mandy
                 10
                 nil
                 (list (pd::msg 20)
                       (pd::msg 30))))        ; pdx:connect arguments work exactly the same as the node creation arguments,
                                              ; except there's no keyword arguments allowed.
  )

;; 4) patch options
(pdx:with-patch
    ("patch-options.pd"
     :width 320 :height 240           ; these are the window sizes.
     :view-width 200 :view-height 100 ; this is the "graph-on-parent" view box.
                                      ; the auto-layouter automatically puts all nodes below this box
                                      ; unless a node has :x and :y values set manually.
     :graph-on-parent t               ; use the view box.
     :hide-object-name nil            ; hide patch and object names in parent patch.
     )
  (pd::text "this is above me.")
  (pd::text :x 20 :y 20 "this is below me."))

;; 5) extras
(pdx:with-patch
    ("extras.pd")
  (pd::bng :bg-color (pdx:color/file 255 0 255) ; initial colors (set at compile-time) are specified using pdx:color/file.
           :fg-color (pdx:color/file 255 255 0)
           :receive "the-bang")
  (pd::msg "; the-bang color "
           (pdx:color/live 255 255 255)         ; dynamic colors (set during run-time) need to use pdx:color/live.
           (pdx:color/live 0 0 0)
           -1))

;; 6) tips and trick
(defun osc-tree (count)
  (if (= count 0)
      (pd::osc~ (random 1000))
      (pd::+~ (osc-tree (1- count))
              (osc-tree (1- count))))) ; generating recursive structures works nicely when returning nodes or ports.
(pdx:with-patch
    ("recursion.pd")
  (let ((tree (pd::*~ .1 (osc-tree 3))))
    (pd::dac~ tree tree)))

;; --------------------------------------------------------------------------------
;; fun with recursion
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

;; --------------------------------------------------------------------------------
;; testing colors
(pdx::with-patch ("color-range.pd" :width 1040 :height 128)
  (mapcar (lambda (r x)
            (pd::bng :x x :y 0
                     :bg-color (pdx::color/file r 0 0))
            (pd::bng :x x :y 16
                     :bg-color (pdx::color/file 0 r 0))
            (pd::bng :x x :y 32
                     :bg-color (pdx::color/file 0 0 r))
            (pd::bng :x x :y 48
                     :bg-color (pdx::color/file r r 0))
            (pd::bng :x x :y 64
                     :bg-color (pdx::color/file r 0 r))
            (pd::bng :x x :y 80
                     :bg-color (pdx::color/file 0 r r))
            (pd::bng :x x :y 96
                     :bg-color (pdx::color/file r r r)))
          (loop
             for r from 0 to 255 by 4
             collect r)
          (loop
             for x from 0 to 63
             collect (* x 16))))

;; --------------------------------------------------------------------------------
;; checking language verbosity, readability on slightly more complex patch
(pdx:with-patch ("list-unwind.pd")
  ;; take list on inlet,
  ;; split list into single items,
  ;; output each item on leftmost outlet,
  ;; bang rightmost outlet when done.
  ;;
  ;; use: feeding variable-length lists as arguments to message boxes with fixed numbers of arguments.
  (let* ((t-in (pd::t "b a" (pd::inlet))) ; inlet
         (split (pd::list "split 1"))
         (t-done (pd::t "b b" (pdx::port 2 split)))
         (buf (pd::list
               (pd::until t-in
                          t-done)
               (pdx::port 1 t-in)))
         (items (pd::outlet :x 0 split)) ; left outlet
         (complete (pd::outlet :x 100 (pdx::port 1 t-done)))) ; right outlet
    (pdx::connect buf nil (pdx::port 1 split))
    (pdx::connect split buf)))

;; --------------------------------------------------------------------------------
;; various miscellaneous things
(pdx:with-patch ("test.pd" :graph-on-parent t
                           :view-width 300
                           :view-height 100
                           ;; :hide-object-name t
                           )
  (let* ((my-adder (pd::+ 1))
         (my-flt (pd::float 0
                            (pd::metro 500
                                       (pd::loadbang))
                            my-adder))
         (list-of-messages (loop for i from 0 to 10 collect
                                (pd::msg i))))
    (pd::print list-of-messages)
    (pdx:connect my-adder my-flt)
    (pd::symbol "$1 $2 $3")
    (pd::print "yeah"
               (pd::msg "message says hello $1" my-flt))
    (pd::outlet my-flt)
    (pd::cnv :x 0 :y 0 :width 500)      ; canvas
    (pd::+ :x 100 :y 0 123 456)         ; manual positioning
    (pd::print "food"
               (pd::bng :x 50 :y 50 :size 32
                        :interrupt 5 :hold 250))
    ))
