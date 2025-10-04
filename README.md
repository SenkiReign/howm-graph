# Howm Graph 

<img width="1000" height="531" alt="14" src="https://github.com/user-attachments/assets/6818f20a-82f7-4415-bf7d-8c08e1b3d3d5" />


Visualize your [Howm](https://github.com/kaorahi/howm) notes as a graph.

- Interactive graph of Howm notes.
- Links automatically created from `>>>keyword` references.
- Lightweight, minimal, and self-contained.
- Node sizes and clustering reflect the number of connections.
- Works fully offline after initial setup.

## Installation

1. Copy `howm-graph.el` to your Emacs `load-path`.  
2. Add to your `init.el` or `config.el`:

   ```elisp
   (load "~/path/to/howm-graph.el")

## Set your Howm directory 

```(setq howm-graph-notes-directory "~/howm") ```

## Generate a Graph

```M-x howm-graph-view```
