# CL-PD

Common Lisp helpers for generating PureData patches. WIP - please excuse the mess!

Targeted at Pd 0.47.1

# Usage

read and evaluate `usage.lisp`

# Tutorial

col1 | col2
---|---
```lisp
(pdx:with-patch             ; with-patch writes a file.
    ("basic-usage.pd")      ; these are options, e.g. file name.
  (pd::text "hello world")) ; all nodes are accessible in the pd package. use pd::node because some overwrite/shadow cl-user (e.g. list).
```
| other content

![Basic Usage](https://pdikmann.github.io/cl-pd/basic-usage.png)
[Basic Usage 2](https://pdikmann.github.io/cl-pd/basic-usage.png)
[Basic Usage 3](docs/basic-usage.png)

# Roadmap

- [DONE] add :view-width and :view-height keywords to with-patch to enable graph-on-parent.
  this should add a "restore"-line to the file
  and add an y-offset to the auto-layouted node positions.
- [DONE] add :x and :y keywords to object nodes for manual placement
- [IN PROGRESS] add gui-nodes like sliders (hsl, vsl), bangs etc. pp. that have dozens of parameters
- write a good tutorial/documentation (with pictures).
- add the missing node types (in order of importance): 
  subpatches,
  arrays & graphs, 
  floatatoms, 
  symbolatoms, 
  structs, 
  [DONE] comments.
- look into asdf (or similar) and quicklisp
- check for reasonable improvements to the auto-layouter (look at graphviz for inspiration)

# Reference

[PureData File Format documentation](http://puredata.info/docs/developer/PdFileFormat#6)
