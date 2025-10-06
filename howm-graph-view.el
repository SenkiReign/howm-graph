;;; howm-graph-view.el --- Graph visualization for Howm notes -*- lexical-binding: t; -*-

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
    
    ;; First pass: create all note nodes and index them by heading
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
                  (let* ((clean-heading (replace-regexp-in-string ">>>\\s-*[[:alnum:]-]+" "" heading))
                         (note-id (format "note%d" note-counter))
                         (note-key (concat heading "__" fname)))
                    
                    (push (list (cons 'id note-id)
                                (cons 'heading clean-heading)
                                (cons 'original-heading heading)
                                (cons 'content (or body ""))
                                (cons 'file fname)
                                (cons 'type "note")
                                (cons 'color "#1f77b4"))
                          nodes)
                    
                    ;; Index by both original and clean heading for lookup
                    (puthash note-key note-id note-ids)
                    (puthash (string-trim clean-heading) note-id note-ids)
                    (setq note-counter (1+ note-counter))))))))))
    
    ;; Second pass: create links
    (dolist (node nodes)
      (let* ((note-id (cdr (assoc 'id node)))
             (heading (cdr (assoc 'original-heading node)))
             (body (cdr (assoc 'content node)))
             (text (concat heading "\n" (or body ""))))
        
        (with-temp-buffer
          (insert text)
          (goto-char (point-min))
          (while (re-search-forward ">>>\\s-*\\([[:alnum:]-]+\\)" nil t)
            (let* ((kw (match-string 1))
                   (target-note-id (gethash (string-trim kw) note-ids)))
              
              (if target-note-id
                  ;; Link to existing note
                  (push (list (cons 'source note-id)
                              (cons 'target target-note-id))
                        links)
                ;; Create keyword node if doesn't exist
                (progn
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
                  ;; Link to keyword
                  (push (list (cons 'source note-id)
                              (cons 'target (gethash kw keyword-ids)))
                        links))))))))
    
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
body { margin: 0; display: flex; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; height: 100vh; overflow: hidden; background: #fafafa; }
#graph { flex: 1; background: white; position: relative; transition: background 0.3s; }
#sidebar { width: 300px; min-width: 200px; max-width: 600px; border-left: 1px solid #ddd; padding: 20px; overflow-y: auto; background: white; box-shadow: -2px 0 8px rgba(0,0,0,0.05); position: relative; }
#resizer { position: absolute; left: 0; top: 0; bottom: 0; width: 5px; cursor: ew-resize; background: transparent; z-index: 10; }
#resizer:hover { background: #4a90e2; }
h2 { margin-top: 0; font-size: 1.2em; color: #333; border-bottom: 2px solid #4a90e2; padding-bottom: 8px; }
h3 { margin-top: 0.5em; margin-bottom: 0.5em; color: #2c3e50; font-size: 1.1em; }
.link { stroke-opacity: 0.4; }
.button-group { display: flex; gap: 8px; margin: 15px 0; }
.btn { 
  flex: 1;
  padding: 10px 15px; 
  background: #4a90e2; 
  color: white; 
  border: none; 
  border-radius: 6px; 
  cursor: pointer; 
  font-size: 0.9em;
  transition: background 0.3s;
}
.btn:hover { background: #357abd; }
.btn.secondary { background: #95a5a6; }
.btn.secondary:hover { background: #7f8c8d; }
.btn.secondary.active { background: #e74c3c; }
.btn.secondary.active:hover { background: #c0392b; }
#stats { color: #666; line-height: 1.8; }
#details { margin-top: 15px; padding: 15px; background: #f9f9f9; border-radius: 8px; border-left: 4px solid #4a90e2; }
body.dark { background: #0d1117; color: #c9d1d9; }
body.dark #graph { background: #000; }
body.dark #sidebar { background: #161b22; border-left-color: #30363d; box-shadow: -2px 0 8px rgba(0,0,0,0.3); }
body.dark h2 { color: #c9d1d9; border-bottom-color: #58a6ff; }
body.dark h3 { color: #e6edf3; }
body.dark #stats { color: #8b949e; }
body.dark #details { background: #0d1117; border-left-color: #58a6ff; }
body.dark .btn { background: #21262d; color: #c9d1d9; }
body.dark .btn:hover { background: #30363d; }
body.dark .btn.secondary { background: #30363d; }
body.dark .btn.secondary:hover { background: #484f58; }
</style>
</head>
<body>
<div id=\"graph\"></div>
<div id=\"sidebar\">
  <div id=\"resizer\"></div>
  <h2>Graph Stats</h2>
  <div id=\"stats\"></div>
  <div class=\"button-group\">
    <button id=\"darkmode-toggle\" class=\"btn\">Dark Mode</button>
    <button id=\"orphan-toggle\" class=\"btn secondary\">Hide Orphans</button>
  </div>
  <h2>Search</h2>
  <input type=\"text\" id=\"search\" placeholder=\"Search nodes...\" style=\"width: 100%; padding: 6px 8px; border: 1px solid #ddd; border-radius: 4px; font-size: 0.85em; margin-bottom: 8px;\">
  <div id=\"search-results\" style=\"font-size: 0.8em; color: #666; margin-bottom: 10px;\"></div>
  <h2>Details</h2>
  <div id=\"details\">Click a node</div>
</div>
<script>
const graph = GRAPH_DATA_HERE;
let width = window.innerWidth - 300;
let height = window.innerHeight;
const svg = d3.select(\"#graph\").append(\"svg\").attr(\"width\", width).attr(\"height\", height);
const container = svg.append(\"g\");
const resizer = document.getElementById(\"resizer\");
const sidebar = document.getElementById(\"sidebar\");
let isResizing = false;
resizer.addEventListener(\"mousedown\", (e) => { isResizing = true; });
document.addEventListener(\"mousemove\", (e) => {
  if (!isResizing) return;
  const newWidth = window.innerWidth - e.clientX;
  if (newWidth >= 200 && newWidth <= 600) {
    sidebar.style.width = newWidth + \"px\";
    width = window.innerWidth - newWidth;
    svg.attr(\"width\", width);
    simulation.force(\"center\", d3.forceCenter(width/2, height/2));
    simulation.alpha(0.3).restart();
  }
});
document.addEventListener(\"mouseup\", () => { isResizing = false; });
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
const link = container.append(\"g\").attr(\"class\",\"link\").selectAll(\"line\").data(graph.links).join(\"line\").attr(\"stroke-width\", d => {
  const sourceNode = graph.nodes.find(n => n.id === (d.source.id || d.source));
  const targetNode = graph.nodes.find(n => n.id === (d.target.id || d.target));
  return (sourceNode.type === \"note\" && targetNode.type === \"note\") ? 2 : 1.5;
}).attr(\"stroke\", d => {
  const sourceNode = graph.nodes.find(n => n.id === (d.source.id || d.source));
  const targetNode = graph.nodes.find(n => n.id === (d.target.id || d.target));
  if (sourceNode.type === \"note\" && targetNode.type === \"note\") return \"#4a90e2\";
  return (sourceNode.type === \"note\" && targetNode.type === \"keyword\") ? \"#f39c12\" : \"#95a5a6\";
});
const node = container.append(\"g\").attr(\"stroke\", \"rgba(255,255,255,0.8)\").attr(\"stroke-width\", 1.5).selectAll(\"circle\").data(graph.nodes).join(\"circle\").attr(\"r\", d => d.type === \"keyword\" ? 10 : 8).attr(\"fill\", d => d.type === \"keyword\" ? \"#f39c12\" : \"#4a90e2\").style(\"cursor\", \"pointer\").call(drag(simulation)).on(\"click\", showDetails).on(\"mouseover\", function() { d3.select(this).attr(\"r\", d => (d.type === \"keyword\" ? 10 : 8) * 1.3); }).on(\"mouseout\", function() { d3.select(this).attr(\"r\", d => d.type === \"keyword\" ? 10 : 8); });
const labels = container.append(\"g\").selectAll(\"text\").data(graph.nodes).join(\"text\").text(d => d.heading.replace(/>>>[\w-]+/g, \"\").trim()).attr(\"font-size\", \"11px\").attr(\"font-weight\", \"500\").attr(\"text-anchor\", \"middle\").attr(\"dy\", \"-12px\").attr(\"pointer-events\", \"none\").attr(\"fill\", d => document.body.classList.contains(\"dark\") ? \"#e6edf3\" : \"#2c3e50\");
simulation.on(\"tick\", () => {
  link.attr(\"x1\", d => d.source.x).attr(\"y1\", d => d.source.y).attr(\"x2\", d => d.target.x).attr(\"y2\", d => d.target.y);
  node.attr(\"cx\", d => d.x).attr(\"cy\", d => d.y);
  labels.attr(\"x\", d => d.x).attr(\"y\", d => d.y);
});
svg.call(d3.zoom().scaleExtent([0.2, 4]).on(\"zoom\", (event) => container.attr(\"transform\", event.transform)));
document.getElementById(\"darkmode-toggle\").addEventListener(\"click\", () => {
  document.body.classList.toggle(\"dark\");
  labels.attr(\"fill\", d => document.body.classList.contains(\"dark\") ? \"#e6edf3\" : \"#2c3e50\");
});
let orphansHidden = false;
document.getElementById(\"orphan-toggle\").addEventListener(\"click\", function() {
  orphansHidden = !orphansHidden;
  this.textContent = orphansHidden ? \"Show Orphans\" : \"Hide Orphans\";
  this.classList.toggle(\"active\", orphansHidden);
  node.attr(\"opacity\", d => (orphansHidden && d.degree === 0) ? 0 : 1);
  labels.attr(\"opacity\", d => (orphansHidden && d.degree === 0) ? 0 : 1);
});
document.getElementById(\"search\").addEventListener(\"input\", (e) => {
  const query = e.target.value.toLowerCase().trim();
  if (query === \"\") {
    node.attr(\"opacity\", 1).attr(\"r\", d => d.type === \"keyword\" ? 10 : 8);
    link.attr(\"opacity\", 0.4);
    labels.attr(\"opacity\", 1);
    document.getElementById(\"search-results\").innerHTML = \"\";
    return;
  }
  const matches = graph.nodes.filter(n => n.heading.toLowerCase().includes(query));
  const matchIds = new Set(matches.map(n => n.id));
  const connectedIds = new Set();
  graph.links.forEach(l => {
    const sourceId = l.source.id || l.source;
    const targetId = l.target.id || l.target;
    if (matchIds.has(sourceId)) connectedIds.add(targetId);
    if (matchIds.has(targetId)) connectedIds.add(sourceId);
  });
  node.attr(\"opacity\", d => matchIds.has(d.id) ? 1 : (connectedIds.has(d.id) ? 0.5 : 0.1))
      .attr(\"r\", d => matchIds.has(d.id) ? (d.type === \"keyword\" ? 12 : 10) : (d.type === \"keyword\" ? 10 : 8));
  link.attr(\"opacity\", l => {
    const sourceId = l.source.id || l.source;
    const targetId = l.target.id || l.target;
    return (matchIds.has(sourceId) || matchIds.has(targetId)) ? 0.6 : 0.05;
  });
  labels.attr(\"opacity\", d => matchIds.has(d.id) ? 1 : (connectedIds.has(d.id) ? 0.6 : 0.2));
  document.getElementById(\"search-results\").innerHTML = `Found ${matches.length} node(s)`;
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
