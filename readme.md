# CL-PD

Common Lisp helpers for generating PureData patches. WIP - please excuse the mess!

# Usage

evaluate `usage.lisp`

# Roadmap

- add the missing node types (in order of importance): 
  subpatches,
  arrays & graphs, 
  floatatoms, 
  symbolatoms, 
  structs, 
  comments.
- add gui-nodes like sliders (hsl, vsl), bangs etc. pp. that have dozens of parameters
- add option to position nodes explicitly (:x :y keywords)
- check for reasonable improvements to the auto-layouter (look at graphviz for inspiration)

# Reference

[PureData File Format documentation](http://puredata.info/docs/developer/PdFileFormat#6)
