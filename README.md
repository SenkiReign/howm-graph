# Howm Graph Viewer

Visualize your [Howm](https://github.com/kaorahi/howm) notes as an interactive, web-based graph using Emacs and D3.js.

## Features

- Interactive graph of Howm notes and keywords.
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
