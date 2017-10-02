# CL-PD

Common Lisp helpers for generating PureData patches. WIP - please excuse the mess!

Targeted at Pd 0.47.1

# Usage

read and evaluate `usage.lisp`

# Tutorial

![basic-usage](https://pdikmann.github.io/cl-pd/basic-usage.png)

```lisp
;; 1) basic usage
(pdx:with-patch             ; with-patch writes a file.
    ("basic-usage.pd")      ; these are options, e.g. file name.
  (pd::text "hello world")) ; all nodes are accessible in the pd package. 
                            ; (use of the "::"-qualifier is a workaround because 
                            ; many PureData names would shadow core CL functions.)
```

![auto-layouter](https://pdikmann.github.io/cl-pd/auto-layouter.png)

```common lisp
;; 2) auto layouter
(pdx:with-patch
    ("auto-layouter.pd"
     :width 200
     :height 200)
  (pd::print "result"
             (pd::+ (pd::msg 1)
                    (pd::msg 2)))) ; connections are laid out by a primitive auto-layouter 
                                   ; (so you can still figure out what's going on).
```

![node-arguments](https://pdikmann.github.io/cl-pd/node-arguments.png)

```
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
```

![patch-options](https://pdikmann.github.io/cl-pd/patch-options.png)

```
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
```

![extras](https://pdikmann.github.io/cl-pd/extras.png)

```
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
```

![recursion](https://pdikmann.github.io/cl-pd/recursion.png)

```
;; 6) tips and tricks: recursion
(defun osc-tree (count)
  (if (= count 0)
      (pd::osc~ (random 1000))
      (pd::+~ (osc-tree (1- count))
              (osc-tree (1- count))))) ; generating recursive structures works nicely when returning nodes or ports.

(pdx:with-patch
    ("recursion.pd")
  (let ((tree (pd::*~ .1 (osc-tree 3))))
    (pd::dac~ tree tree)))
```

# Roadmap

- [x] add :view-width and :view-height keywords to with-patch to enable graph-on-parent.
  this should add a "restore"-line to the file
  and add an y-offset to the auto-layouted node positions.
- [X] add :x and :y keywords to object nodes for manual placement
- [ ] add gui-nodes like sliders (hsl, vsl), bangs etc. pp. that have dozens of parameters
- [ ] write a good tutorial/documentation (with pictures).
- add the missing node types (in order of importance): 
  - [ ] subpatches,
  - [ ] arrays & graphs, 
  - [x] floatatoms, 
  - [x] symbolatoms, 
  - [ ] structs, 
  - [x] comments.
- [ ] look into asdf (or similar) and quicklisp
- [ ] check for reasonable improvements to the auto-layouter (look at graphviz for inspiration)

# Reference

[PureData File Format documentation](http://puredata.info/docs/developer/PdFileFormat#6)
