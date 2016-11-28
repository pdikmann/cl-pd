# CL-PD

Common Lisp helpers for generating PureData patches. WIP - please excuse the mess!

Targeted at Pd 0.47.1

# Usage

evaluate `usage.lisp`

# Roadmap

- [DONE] add :view-width and :view-height keywords to with-patch to enable graph-on-parent.
  this should add a "restore"-line to the file
  and add an y-offset to the auto-layouted node positions.
- [DONE] add :x and :y keywords to object nodes for manual placement
- [IN PROGRESS] add gui-nodes like sliders (hsl, vsl), bangs etc. pp. that have dozens of parameters
- add the missing node types (in order of importance): 
  subpatches,
  arrays & graphs, 
  floatatoms, 
  symbolatoms, 
  structs, 
  comments.
- check for reasonable improvements to the auto-layouter (look at graphviz for inspiration)

# Reference

[PureData File Format documentation](http://puredata.info/docs/developer/PdFileFormat#6)
