;;; howm-graph-view.el --- Graph visualization for Howm notes -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Your Name
;; Version: 1.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: howm, graph, visualization

;;; Commentary:
;; Graph visualization for Howm notes.

;;; Code:

(setq coding-system-for-read 'utf-8
      coding-system-for-write 'utf-8
      default-buffer-file-coding-system 'utf-8
      locale-coding-system 'utf-8
      selection-coding-system 'utf-8
      inhibit-eol-conversion t)



(require 'json)
(require 'url)


(defgroup howm-graph nil
  "Graph visualization for Howm notes."
  :group 'howm
  :prefix "howm-graph-")

(defcustom howm-graph-notes-directory "~/howm"
  "Directory containing Howm notes."
  :type 'directory
  :group 'howm-graph)

(defcustom howm-graph-output-file (expand-file-name "~/.emacs.d/howm-graph.html")
  "Path to the generated graph HTML file."
  :type 'file
  :group 'howm-graph)

(defcustom howm-graph-d3-cache-file (expand-file-name "~/.emacs.d/howm-d3.js")
  "Path to cached D3.js library."
  :type 'file
  :group 'howm-graph)

(defun howm-graph--parse-notes ()
  "Parse all Howm notes and return nodes and links."
  (let ((nodes '())
        (links '())
        (note-ids (make-hash-table :test 'equal))
        (keyword-ids (make-hash-table :test 'equal))
        (note-counter 0)
        (keyword-counter 0)
        (files (directory-files howm-graph-notes-directory t "\\.txt\\'")))
    
    (dolist (file files)
      (let* ((fname (file-name-nondirectory file))
             (content (with-temp-buffer
                        (insert-file-contents file)
                        (buffer-string)))
             (sections (split-string content "^=" t)))
        
        (let ((i 0))
          (dolist (section sections)
            (setq i (1+ i))
            (when (or (> i 1) (not (string-match-p "\\`[[:space:]\n]*\\'" section)))
              (let* ((lines (split-string section "\n" t))
                     (heading (when lines (string-trim (car lines))))
                     (body (when (cdr lines)
                             (string-trim (mapconcat 'identity (cdr lines) "\n")))))
                
                (when heading
                  (let ((note-id (format "note%d" note-counter))
                        (note-key (concat heading "__" fname)))
                    
                    (push (list (cons 'id note-id)
                                (cons 'heading (replace-regexp-in-string ">>>\\s-*[[:alnum:]-]+" "" heading))
                                (cons 'content (or body ""))
                                (cons 'file fname)
                                (cons 'type "note")
                                (cons 'color "#1f77b4"))
                          nodes)
                    
                    (puthash note-key note-id note-ids)
                    (setq note-counter (1+ note-counter))
                    
                    (let ((text (concat heading "\n" (or body ""))))
                      (with-temp-buffer
                        (insert text)
                        (goto-char (point-min))
                        (while (re-search-forward ">>>\\s-*\\([[:alnum:]-]+\\)" nil t)
                          (let ((kw (match-string 1)))
                            (unless (gethash kw keyword-ids)
                              (let ((kw-id (format "kw%d" keyword-counter)))
                                (push (list (cons 'id kw-id)
                                            (cons 'heading kw)
                                            (cons 'content "")
                                            (cons 'file "")
                                            (cons 'type "keyword")
                                            (cons 'color "#ff7f0e"))
                                      nodes)
                                (puthash kw kw-id keyword-ids)
                                (setq keyword-counter (1+ keyword-counter))))
                            
                            (push (list (cons 'source note-id)
                                        (cons 'target (gethash kw keyword-ids)))
                                  links)))))))))))))
    
    (list :nodes (nreverse nodes) :links (nreverse links))))

(defun howm-graph--fetch-d3 ()
  "Fetch D3.js library, using cache if available."
  (if (file-exists-p howm-graph-d3-cache-file)
      (with-temp-buffer
        (insert-file-contents howm-graph-d3-cache-file)
        (buffer-string))
    (message "Downloading D3.js (one-time download)...")
    (let ((d3-url "https://d3js.org/d3.v7.min.js"))
      (with-current-buffer (url-retrieve-synchronously d3-url t)
        (goto-char (point-min))
        (re-search-forward "^$")
        (let ((d3-content (buffer-substring (point) (point-max))))
          (with-temp-file howm-graph-d3-cache-file
            (insert d3-content))
          (message "D3.js cached to %s" howm-graph-d3-cache-file)
          d3-content)))))

(defun howm-graph--html-template ()
  "Return the HTML template as a string."
  "<!DOCTYPE html>
<html lang=\"en\">
<head>
<meta charset=\"UTF-8\">
<title>Howm Graph</title>
<script>D3_SCRIPT_PLACEHOLDER</script>
<style>
body { margin: 0; display: flex; font-family: sans-serif; height: 100vh; overflow: hidden; }
#graph { flex: 3; }
#sidebar { flex: 1; border-left: 1px solid #ccc; padding: 10px; overflow-y: auto; }
h2, h3 { margin-top: 0.5em; margin-bottom: 0.3em; }
.link { stroke-opacity: 0.6; }
#darkmode-toggle { margin: 10px 0; }
body.dark { background: #121212; color: #eee; }
body.dark #sidebar { background: #1e1e1e; color: #eee; }
</style>
</head>
<body>
<div id=\"graph\"></div>
<div id=\"sidebar\">
  <h2>Graph Stats</h2>
  <div id=\"stats\"></div>
  <button id=\"darkmode-toggle\">Toggle Dark Mode</button>
  <h2>Details</h2>
  <div id=\"details\">Click a node</div>
</div>
<script>
const graph = GRAPH_DATA_HERE;
const width = window.innerWidth * 0.75;
const height = window.innerHeight;
const svg = d3.select(\"#graph\").append(\"svg\").attr(\"width\", width).attr(\"height\", height);
const container = svg.append(\"g\");
graph.nodes.forEach(n => n.degree = 0);
graph.links.forEach(l => {
  const s = typeof l.source === \"object\" ? l.source.id : l.source;
  const t = typeof l.target === \"object\" ? l.target.id : l.target;
  graph.nodes.find(n => n.id === s).degree += 1;
  graph.nodes.find(n => n.id === t).degree += 1;
});
const avgDegree = (graph.links.length * 2 / graph.nodes.length).toFixed(2);
document.getElementById(\"stats\").innerHTML = `<p><b>Nodes:</b> ${graph.nodes.length}<br><b>Links:</b> ${graph.links.length}<br><b>Avg degree:</b> ${avgDegree}</p>`;
const simulation = d3.forceSimulation(graph.nodes)
  .force(\"link\", d3.forceLink(graph.links).id(d => d.id).distance(d => d.target.type === \"keyword\" ? 80 : 100).strength(d => d.target.type === \"keyword\" ? 0.7 : 0.5))
  .force(\"charge\", d3.forceManyBody().strength(d => -150 - (d.degree || 0)*15))
  .force(\"center\", d3.forceCenter(width/2, height/2).strength(0.1))
  .force(\"collide\", d3.forceCollide(d => d.type === \"keyword\" ? 20 : 18))
  .force(\"x\", d3.forceX(width/2).strength(0.05))
  .force(\"y\", d3.forceY(height/2).strength(0.05));
const link = container.append(\"g\").attr(\"class\",\"link\").selectAll(\"line\").data(graph.links).join(\"line\").attr(\"stroke-width\", 1).attr(\"stroke\", d => {
  const sourceNode = graph.nodes.find(n => n.id === (d.source.id || d.source));
  const targetNode = graph.nodes.find(n => n.id === (d.target.id || d.target));
  return (sourceNode.type === \"note\" && targetNode.type === \"keyword\") ? \"#ff7f0e\" : \"#999\";
});
const node = container.append(\"g\").attr(\"stroke\", \"#fff\").attr(\"stroke-width\", 1.5).selectAll(\"circle\").data(graph.nodes).join(\"circle\").attr(\"r\", d => d.type === \"keyword\" ? 8 : 6).attr(\"fill\", d => d.color).call(drag(simulation)).on(\"click\", showDetails);
const labels = container.append(\"g\").selectAll(\"text\").data(graph.nodes).join(\"text\").text(d => d.heading.replace(/>>>[\w-]+/g, \"\").trim()).attr(\"font-size\", \"10px\").attr(\"text-anchor\", \"middle\").attr(\"dy\", \"-10px\").attr(\"pointer-events\", \"none\").attr(\"fill\", d => document.body.classList.contains(\"dark\") ? \"#eee\" : \"#000\");
simulation.on(\"tick\", () => {
  link.attr(\"x1\", d => d.source.x).attr(\"y1\", d => d.source.y).attr(\"x2\", d => d.target.x).attr(\"y2\", d => d.target.y);
  node.attr(\"cx\", d => d.x).attr(\"cy\", d => d.y);
  labels.attr(\"x\", d => d.x).attr(\"y\", d => d.y);
});
svg.call(d3.zoom().scaleExtent([0.2, 4]).on(\"zoom\", (event) => container.attr(\"transform\", event.transform)));
document.getElementById(\"darkmode-toggle\").addEventListener(\"click\", () => {
  document.body.classList.toggle(\"dark\");
  labels.attr(\"fill\", d => document.body.classList.contains(\"dark\") ? \"#eee\" : \"#000\");
});
function showDetails(event, d) {
  document.getElementById(\"details\").innerHTML = `<h3>${d.heading.replace(/>>>[\w-]+/g, \"\").trim()}</h3><div style=\"white-space: pre-wrap; word-wrap: break-word;\">${d.content}</div><p><em>${d.file}</em></p>`;
}
function drag(simulation) {
  function dragstarted(event, d) { if (!event.active) simulation.alphaTarget(0.3).restart(); d.fx = d.x; d.fy = d.y; }
  function dragged(event, d) { d.fx = event.x; d.fy = event.y; }
  function dragended(event, d) { if (!event.active) simulation.alphaTarget(0); d.fx = null; d.fy = null; }
  return d3.drag().on(\"start\", dragstarted).on(\"drag\", dragged).on(\"end\", dragended);
}
</script>
</body>
</html>")

(defun howm-graph--generate-html ()
  "Generate standalone HTML file with embedded graph data."
  (let* ((graph-data (howm-graph--parse-notes))
         (json-encoding-pretty-print nil)
         (graph-json (json-encode graph-data))
         (num-nodes (length (plist-get graph-data :nodes)))
         (num-links (length (plist-get graph-data :links)))
         (html-template (howm-graph--html-template))
         (d3-script (howm-graph--fetch-d3)))
    
    (with-temp-file howm-graph-output-file
      (insert html-template)
      (goto-char (point-min))
      (search-forward "D3_SCRIPT_PLACEHOLDER")
      (replace-match d3-script t t)
      (goto-char (point-min))
      (search-forward "GRAPH_DATA_HERE")
      (replace-match graph-json t t))
    
    (message "Generated %s with %d nodes and %d links"
             howm-graph-output-file num-nodes num-links)
    howm-graph-output-file))






;;;###autoload
(defun howm-graph-debug ()
  "Debug function to test parsing."
  (interactive)
  (let ((graph-data (howm-graph--parse-notes)))
    (with-current-buffer (get-buffer-create "*howm-graph-debug*")
      (erase-buffer)
      (insert (format "Nodes: %d\n" (length (plist-get graph-data :nodes))))
      (insert (format "Links: %d\n\n" (length (plist-get graph-data :links))))
      (insert "Links grouped by source:\n")
      (let ((link-counts (make-hash-table :test 'equal)))
        (dolist (link (plist-get graph-data :links))
          (let ((src (cdr (assoc 'source link))))
            (puthash src (1+ (gethash src link-counts 0)) link-counts)))
        (maphash (lambda (k v)
                   (when (> v 1)
                     (insert (format "  %s: %d links\n" k v))))
                 link-counts))
      (switch-to-buffer (current-buffer)))))

;;;###autoload
(defun howm-graph-view ()
  "Generate and display the Howm notes graph in browser."
  (interactive)
  (message "Generating Howm graph...")
  (let* ((graph-data (howm-graph--parse-notes))
         (nodes (plist-get graph-data :nodes))
         (links (plist-get graph-data :links))
         (note-nodes (seq-filter (lambda (n) (equal (cdr (assoc 'type n)) "note")) nodes))
         (keyword-nodes (seq-filter (lambda (n) (equal (cdr (assoc 'type n)) "keyword")) nodes)))
    (message "Parsed: %d notes, %d keywords, %d links" 
             (length note-nodes) (length keyword-nodes) (length links))
    (let ((html-file (howm-graph--generate-html)))
      (browse-url (concat "file://" html-file))
      (message "Opening graph: %s" html-file))))

;;;###autoload
(defun howm-graph-refresh ()
  "Regenerate the graph and reload in browser."
  (interactive)
  (message "Refreshing Howm graph...")
  (howm-graph--generate-html)
  (message "Graph refreshed. The file has been updated."))

(provide 'howm-graph-view)
;;; howm-graph-view.el ends here
