# CL-PD

Common Lisp helpers for generating PureData patches. WIP - please excuse the mess!

# Usage

evaluate `usage.lisp`

# Roadmap

- add :view-width and :view-height keywords to with-patch to enable graph-on-parent.
  this should add a "restore"-line to the file
  and add an y-offset to the auto-layouted node positions.
- add :x and :y keywords to object nodes for manual placement
- add the missing node types: messages, comments, floatatoms, symbolatoms, arrays, structs, subpatches, ...
- add gui-nodes like sliders (hsl, vsl), bangs etc. pp. that have dozens of parameters
- add option to position nodes by hand (:x :y keywords)
- check for reasonable improvements to the auto-layouter (look at graphviz for inspiration)

# Reference

[PureData File Format documentation](http://puredata.info/docs/developer/PdFileFormat#6)
