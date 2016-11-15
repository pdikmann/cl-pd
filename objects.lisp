(defpackage :pd-objects
  (:nicknames :pd)
  (:use :common-lisp)
  (:import-from :pd-structs
                :node
                :object-node)
  (:import-from :pd-writer
                :connect
                :add-node)
  (:shadow :float
           :symbol
           :t
           :print
           :list
           :+
           :-
           :*
           :/
           :<
           :>
           :<=
           :>=
           :mod
           :sin
           :cos
           :tan
           :atan
           :sqrt
           :log
           :exp
           :abs
           :random
           :max
           :min
           :array
           :declare
           :get
           :set
           :append))

(in-package :pd-objects)

(defparameter *object-nodes*
  '( ;; -------- general --------
    bang
    b
    float
    f
    symbol
    int
    i
    send
    s
    receive
    r
    select
    route
    pack
    unpack
    trigger
    t
    spigot
    moses
    until
    print
    makefilename
    change
    swap
    value
    list
    ;; -------- time --------
    delay
    metro
    line
    timer
    cputime
    realtime
    pipe
    ;; -------- math --------
    +
    -
    *
    /
    pow
    ==
    !=
    >
    <
    >=
    <=
    &
    &&
    ||
    ||||
    %
    <<
    >>
    mtof
    powtodb
    rmstodb
    ftom
    dbtopow
    dbtorms
    mod
    div
    sin
    cos
    tan
    atan
    atan2
    sqrt
    log
    exp
    abs
    random
    max
    min
    clip
    wrap
    ;; -------- midi and osc --------
    notein
    ctlin
    pgmin
    bendin
    touchin
    polytouchin
    midiin
    sysexin
    noteout
    ctlout
    pgmout
    bendout
    touchout
    polytouchout
    midiout
    makenote
    stripnote
    oscparse
    oscformat
    ;; -------- arrays / tables --------
    tabread
    tabread4
    tabwrite
    soundfiler
    table
    array
    ;; -------- misc --------
    loadbang
    serial
    netsend
    netreceive
    qlist
    textfile
    text
    openpanel
    savepanel
    bag
    poly
    key
    keyup
    keyname
    declare
    ;; -------- audio math --------
    +~
    -~
    *~
    /~
    max~
    min~
    clip~
    q8_rsqrt~
    q8_sqrt~
    sqrt~
    wrap~
    fft~
    ifft~
    rfft~
    rifft~
    pow~
    log~
    exp~
    abs~
    framp~
    mtof~
    ftom~
    rmstodb~
    dbtorms~
    ;; -------- general audio manipulation --------
    dac~
    adc~
    sig~
    line~
    vline~
    threshold~
    snapshot~
    vsnapshot~
    bang~
    samplerate~
    send~
    receive~
    throw~
    catch~
    block~
    switch~
    readsf~
    writesf~
    ;; -------- audio oscillators and tables --------
    phasor~
    cos~
    osc~
    tabwrite~
    tabplay~
    tabread~
    tabread4~
    tabosc4~
    tabsend~
    tabreceive~
    ;; -------- audio filters --------
    vcf~
    noise~
    env~
    hip~
    lop~
    bp~
    biquad~
    samphold~
    print~
    rpole~
    rzero~
    rzero_rev~
    cpole~
    czero~
    czero_rev~
    ;; -------- audio delay --------
    delwrite~
    delread~
    vd~
    ;; -------- subwindows --------
    ;; pd
    inlet
    outlet
    inlet~
    outlet~
    ;; -------- data templates --------
    struct
    drawcurve
    filledcurve
    drawpolygon
    filledpolygon
    plot
    drawnumber
    ;; -------- accessing data --------
    pointer
    get
    set
    element
    getsize
    setsize
    append
    scalar
    ;; -------- extras --------
    sigmund~
    bonk~
    choice
    hilbert~
    complex-mod~
    expr~
    expr
    fexpr~
    loop~
    lrshift~
    pd~
    rev1~
    rev2~
    rev3~
    bob~
    ))

(defmacro node-template (name)
  `(defun ,name (&rest args)
     (let ((n (make-instance 'object-node
                             :name (format nil "~(~a~)" ',name))))
       (connect n args)
       (add-node n)
       n)))

(defmacro map-templates ()
  `(progn
     ,@(mapcar (lambda (name) `(node-template ,name))
               *object-nodes*)))

(map-templates)
