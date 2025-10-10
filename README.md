# Howm Graph 

Visualize your [Howm](https://github.com/kaorahi/howm) notes as a graph.

- Interactive graph of Howm notes.
- Links automatically created from `>>>keyword` references.
- Lightweight, minimal, and self-contained.
- Clustering reflect the number of connections.
- Works fully offline after initial setup.

<img width="1784" height="914" alt="12" src="https://github.com/user-attachments/assets/e6830d57-2777-4a36-9239-b033ed7727d4" />



## Installation

1. Copy `howm-graph.el` to your Emacs `load-path`.  
2. Add to your `init.el` or `config.el`:

   ```elisp
   (load "~/path/to/howm-graph.el")

## Set your Howm directory 

```(setq howm-graph-notes-directory "~/howm") ```

## Generate a Graph

```M-x howm-graph-view```

## Important Note

- Works only on .txt format
