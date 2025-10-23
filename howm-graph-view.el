;;; howm-graph-view.el --- Graph visualization for Howm notes -*- lexical-binding: t; -*-

;;; Commentary:
;; Graph visualization for Howm notes.

;;; Code:

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
                    
                    (puthash note-key note-id note-ids)
                    (puthash (string-trim clean-heading) note-id note-ids)
                    (setq note-counter (1+ note-counter))))))))))
    
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
                  (push (list (cons 'source note-id)
                              (cons 'target target-note-id))
                        links)
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

(defun howm-graph--make-html-content ()
  "Generate the HTML content."
  (concat
   "<!DOCTYPE html>\n"
   "<html>\n"
   "<head>\n"
   "<meta charset=\"UTF-8\">\n"
   "<title>Howm Graph</title>\n"
   "<script>REPLACE_D3_HERE</script>\n"
   "<style>\n"
   "body { margin: 0; display: flex; font-family: sans-serif; height: 100vh; overflow: hidden; background: white; }\n"
   "#graph { flex: 1; background: white; transition: background 0.3s; }\n"
   "#sidebar { width: 300px; min-width: 200px; max-width: 600px; border-left: 1px solid #ddd; padding: 20px; overflow-y: auto; background: white; position: relative; transition: all 0.3s; }\n"
   "#resizer { position: absolute; left: 0; top: 0; bottom: 0; width: 5px; cursor: ew-resize; background: transparent; z-index: 10; }\n"
   "#resizer:hover { background: #4a90e2; }\n"
   "h2 { margin: 15px 0 10px 0; font-size: 1.1em; color: #333; }\n"
   ".btn { padding: 8px 16px; margin: 5px 5px 5px 0; background: #4a90e2; color: white; border: none; border-radius: 4px; cursor: pointer; flex: 1; }\n"
   ".btn:hover { background: #357abd; }\n"
   ".btn.secondary { background: #95a5a6; }\n"
   ".btn.secondary:hover { background: #7f8c8d; }\n"
   ".btn.active { background: #e74c3c; }\n"
   "#details { padding: 15px; background: #f9f9f9; border-radius: 8px; border-left: 4px solid #1f77b4; line-height: 1.8; }\n"
   "#details h3 { margin-top: 0; margin-bottom: 12px; color: #2c3e50; font-size: 1.15em; word-wrap: break-word; }\n"
   "#details p { margin: 10px 0; white-space: pre-wrap; word-wrap: break-word; overflow-wrap: break-word; }\n"
   "#details em { display: block; margin-top: 15px; padding-top: 10px; border-top: 1px solid #ddd; color: #666; font-size: 0.9em; word-wrap: break-word; }\n"
   "</style>\n"
   "</head>\n"
   "<body>\n"
   "<div id=\"graph\"></div>\n"
   "<div id=\"sidebar\">\n"
   "<div id=\"resizer\"></div>\n"
   "<h2>Controls</h2>\n"
   "<div style=\"display: flex; gap: 5px; margin-bottom: 10px;\">\n"
   "<button id=\"btn-dark\" class=\"btn\">Dark Mode</button>\n"
   "<button id=\"btn-orphan\" class=\"btn secondary\">Hide Orphans</button>\n"
   "<button id=\"btn-label\" class=\"btn secondary\">Hide Labels</button>\n"
   "</div>\n"
   "<h2>Search</h2>\n"
   "<input type=\"text\" id=\"search\" placeholder=\"Search nodes...\" style=\"width: 100%; padding: 8px; border: 1px solid #ddd; border-radius: 4px; margin-bottom: 10px; box-sizing: border-box;\">\n"
   "<div id=\"search-results\" style=\"font-size: 0.9em; color: #666; margin-bottom: 10px;\"></div>\n"
   "<h2>Stats</h2>\n"
   "<div id=\"stats\"></div>\n"
   "<h2>Details</h2>\n"
   "<div id=\"details\">Click a node</div>\n"
   "</div>\n"
   "<script>\n"
   "const graph = REPLACE_DATA_HERE;\n"
   "let width = window.innerWidth - 300;\n"
   "let height = window.innerHeight;\n"
   "const svg = d3.select('#graph').append('svg').attr('width', width).attr('height', height);\n"
   "const container = svg.append('g');\n"
   "const resizer = document.getElementById('resizer');\n"
   "const sidebar = document.getElementById('sidebar');\n"
   "let isResizing = false;\n"
   "resizer.addEventListener('mousedown', () => { isResizing = true; });\n"
   "document.addEventListener('mousemove', (e) => {\n"
   "  if (!isResizing) return;\n"
   "  const newWidth = window.innerWidth - e.clientX;\n"
   "  if (newWidth >= 200 && newWidth <= 600) {\n"
   "    sidebar.style.width = newWidth + 'px';\n"
   "    width = window.innerWidth - newWidth;\n"
   "    svg.attr('width', width);\n"
   "    simulation.force('center', d3.forceCenter(width/2, height/2).strength(0.1));\n"
   "    simulation.force('x', d3.forceX(width/2).strength(0.05));\n"
   "    simulation.alpha(0.3).restart();\n"
   "  }\n"
   "});\n"
   "document.addEventListener('mouseup', () => { isResizing = false; });\n"
   "graph.nodes.forEach(n => n.degree = 0);\n"
   "graph.links.forEach(l => {\n"
   "  const s = typeof l.source === 'object' ? l.source.id : l.source;\n"
   "  const t = typeof l.target === 'object' ? l.target.id : l.target;\n"
   "  graph.nodes.find(n => n.id === s).degree += 1;\n"
   "  graph.nodes.find(n => n.id === t).degree += 1;\n"
   "});\n"
   "document.getElementById('stats').innerHTML = '<p>Nodes: ' + graph.nodes.length + '<br>Links: ' + graph.links.length + '</p>';\n"
   "const simulation = d3.forceSimulation(graph.nodes)\n"
   "  .force('link', d3.forceLink(graph.links).id(d => d.id).distance(d => d.target.type === 'keyword' ? 80 : 100).strength(d => d.target.type === 'keyword' ? 0.7 : 0.5))\n"
   "  .force('charge', d3.forceManyBody().strength(d => -150 - (d.degree || 0)*15))\n"
   "  .force('center', d3.forceCenter(width/2, height/2).strength(0.1))\n"
   "  .force('collide', d3.forceCollide(d => d.type === 'keyword' ? 20 : 18))\n"
   "  .force('x', d3.forceX(width/2).strength(0.05))\n"
   "  .force('y', d3.forceY(height/2).strength(0.05));\n"
   "const link = container.append('g').selectAll('line').data(graph.links).join('line')\n"
   "  .attr('stroke', '#999').attr('stroke-width', 1.5).attr('stroke-opacity', 0.6);\n"
   "const node = container.append('g').selectAll('circle').data(graph.nodes).join('circle')\n"
   "  .attr('r', d => {\n"
   "    const baseSize = d.type === 'keyword' ? 8 : 6;\n"
   "    const scaleFactor = Math.sqrt(d.degree + 1);\n"
   "    return baseSize + scaleFactor * 2;\n"
   "  })\n"
   "  .attr('fill', d => {\n"
   "    if (d.type === 'keyword') return '#ff7f0e';\n"
   "    if (d.degree === 0) return '#95a5a6';\n"
   "    return '#1f77b4';\n"
   "  })\n"
   "  .attr('stroke', '#fff')\n"
   "  .attr('stroke-width', 1.5)\n"
   "  .style('cursor', 'pointer')\n"
   "  .call(d3.drag().on('start', dragstarted).on('drag', dragged).on('end', dragended))\n"
   "  .on('mouseover', function(e, d) {\n"
   "    d3.select(this).attr('stroke-width', 3);\n"
   "    if (hideLabels) {\n"
   "      const label = labels.filter(l => l === d);\n"
   "      label.attr('opacity', 1).attr('font-weight', 'bold');\n"
   "    }\n"
   "  })\n"
   "  .on('mouseout', function(e, d) {\n"
   "    d3.select(this).attr('stroke-width', 1.5);\n"
   "    if (hideLabels) {\n"
   "      const label = labels.filter(l => l === d);\n"
   "      label.attr('opacity', 0).attr('font-weight', 'normal');\n"
   "    }\n"
   "  })\n"
   "  .on('click', (e,d) => {\n"
   "    const content = d.content || 'No content';\n"
   "    const file = d.file || '';\n"
   "    let processedContent = content.replace(/file:\\/\\/\\/([^\\s]+\\.(png|jpg|jpeg|gif|svg|webp))/gi, '<br><img src=\"file:///$1\" style=\"max-width: 100%; height: auto; margin: 10px 0; border-radius: 4px;\"><br>');\n"
   "    document.getElementById('details').innerHTML = '<h3>' + d.heading + '</h3><p>' + processedContent + '</p>' + (file ? '<em>' + file + '</em>' : '');\n"
   "  });\n"
   "const labels = container.append('g').selectAll('text').data(graph.nodes).join('text')\n"
   "  .text(d => d.heading.replace(/>>>\\S+/g, '').trim())\n"
   "  .attr('font-size', '10px').attr('text-anchor', 'middle')\n"
   "  .attr('dy', d => d.type === 'keyword' ? -12 : -10)\n"
   "  .attr('fill', '#000').attr('pointer-events', 'none');\n"
   "simulation.on('tick', () => {\n"
   "  link.attr('x1', d => d.source.x).attr('y1', d => d.source.y).attr('x2', d => d.target.x).attr('y2', d => d.target.y);\n"
   "  node.attr('cx', d => d.x).attr('cy', d => d.y);\n"
   "  labels.attr('x', d => d.x).attr('y', d => d.y);\n"
   "});\n"
   "svg.call(d3.zoom().scaleExtent([0.2, 4]).on('zoom', e => container.attr('transform', e.transform)));\n"
   "let darkMode = false, hideOrphans = false, hideLabels = false;\n"
   "document.getElementById('btn-dark').onclick = () => {\n"
   "  darkMode = !darkMode;\n"
   "  if (darkMode) {\n"
   "    document.body.style.background = '#000';\n"
   "    document.getElementById('graph').style.background = '#000';\n"
   "    document.getElementById('sidebar').style.background = '#1a1a1a';\n"
   "    document.getElementById('sidebar').style.borderLeftColor = '#4a9eff';\n"
   "    document.getElementById('sidebar').style.color = '#b6d7ff';\n"
   "    document.querySelectorAll('#sidebar h2').forEach(h => h.style.color = '#b6d7ff');\n"
   "    document.getElementById('details').style.background = '#0a0a0a';\n"
   "    document.getElementById('details').style.borderLeftColor = '#4a9eff';\n"
   "    document.getElementById('details').style.color = '#b6d7ff';\n"
   "    document.getElementById('search').style.background = '#1a1a1a';\n"
   "    document.getElementById('search').style.color = '#b6d7ff';\n"
   "    document.getElementById('search').style.borderColor = '#4a9eff';\n"
   "    document.getElementById('search-results').style.color = '#b6d7ff';\n"
   "    document.getElementById('stats').style.color = '#b6d7ff';\n"
   "    node.attr('fill', d => {\n"
   "      if (d.type === 'keyword') return '#ffa500';\n"
   "      if (d.degree === 0) return '#7f8c8d';\n"
   "      return '#4a9eff';\n"
   "    });\n"
   "    node.attr('stroke', '#2a2a2a');\n"
   "    link.attr('stroke', '#4a9eff');\n"
   "    labels.attr('fill', '#e0e0e0');\n"
   "  } else {\n"
   "    document.body.style.background = '#fff';\n"
   "    document.getElementById('graph').style.background = '#fff';\n"
   "    document.getElementById('sidebar').style.background = '#fff';\n"
   "    document.getElementById('sidebar').style.borderLeftColor = '#ddd';\n"
   "    document.getElementById('sidebar').style.color = '#2c3e50';\n"
   "    document.querySelectorAll('#sidebar h2').forEach(h => h.style.color = '#333');\n"
   "    document.getElementById('details').style.background = '#f9f9f9';\n"
   "    document.getElementById('details').style.borderLeftColor = '#1f77b4';\n"
   "    document.getElementById('details').style.color = '#2c3e50';\n"
   "    document.getElementById('search').style.background = '#fff';\n"
   "    document.getElementById('search').style.color = '#2c3e50';\n"
   "    document.getElementById('search').style.borderColor = '#ddd';\n"
   "    document.getElementById('search-results').style.color = '#666';\n"
   "    document.getElementById('stats').style.color = '#2c3e50';\n"
   "    node.attr('fill', d => {\n"
   "      if (d.type === 'keyword') return '#ff7f0e';\n"
   "      if (d.degree === 0) return '#95a5a6';\n"
   "      return '#1f77b4';\n"
   "    });\n"
   "    node.attr('stroke', '#fff');\n"
   "    link.attr('stroke', '#999');\n"
   "    labels.attr('fill', '#000');\n"
   "  }\n"
   "};\n"
   "document.getElementById('btn-orphan').onclick = function() {\n"
   "  hideOrphans = !hideOrphans;\n"
   "  this.textContent = hideOrphans ? 'Show Orphans' : 'Hide Orphans';\n"
   "  this.classList.toggle('active', hideOrphans);\n"
   "  node.attr('opacity', d => (hideOrphans && d.degree === 0) ? 0 : 1);\n"
   "  labels.attr('opacity', d => (hideOrphans && d.degree === 0) ? 0 : (hideLabels ? 0 : 1));\n"
   "};\n"
   "document.getElementById('btn-label').onclick = function() {\n"
   "  hideLabels = !hideLabels;\n"
   "  this.textContent = hideLabels ? 'Show Labels' : 'Hide Labels';\n"
   "  this.classList.toggle('active', hideLabels);\n"
   "  labels.attr('opacity', d => {\n"
   "    if (hideLabels) return 0;\n"
   "    if (hideOrphans && d.degree === 0) return 0;\n"
   "    return 1;\n"
   "  });\n"
   "};\n"
   "function dragstarted(e,d) { if (!e.active) simulation.alphaTarget(0.3).restart(); d.fx = d.x; d.fy = d.y; }\n"
   "function dragged(e,d) { d.fx = e.x; d.fy = e.y; }\n"
   "function dragended(e,d) { if (!e.active) simulation.alphaTarget(0); d.fx = null; d.fy = null; }\n"
   "document.getElementById('search').addEventListener('input', (e) => {\n"
   "  const query = e.target.value.toLowerCase().trim();\n"
   "  if (query === '') {\n"
   "    node.attr('opacity', 1);\n"
   "    link.attr('opacity', 0.6);\n"
   "    labels.attr('opacity', hideLabels ? 0 : 1);\n"
   "    document.getElementById('search-results').innerHTML = '';\n"
   "    return;\n"
   "  }\n"
   "  const matches = graph.nodes.filter(n => n.heading.toLowerCase().includes(query));\n"
   "  const matchIds = new Set(matches.map(n => n.id));\n"
   "  const connectedIds = new Set();\n"
   "  graph.links.forEach(l => {\n"
   "    const sourceId = l.source.id || l.source;\n"
   "    const targetId = l.target.id || l.target;\n"
   "    if (matchIds.has(sourceId)) connectedIds.add(targetId);\n"
   "    if (matchIds.has(targetId)) connectedIds.add(sourceId);\n"
   "  });\n"
   "  node.attr('opacity', d => matchIds.has(d.id) ? 1 : (connectedIds.has(d.id) ? 0.5 : 0.1));\n"
   "  link.attr('opacity', l => {\n"
   "    const sourceId = l.source.id || l.source;\n"
   "    const targetId = l.target.id || l.target;\n"
   "    return (matchIds.has(sourceId) || matchIds.has(targetId)) ? 0.8 : 0.05;\n"
   "  });\n"
   "  labels.attr('opacity', d => {\n"
   "    if (hideLabels) return 0;\n"
   "    if (matchIds.has(d.id)) return 1;\n"
   "    if (connectedIds.has(d.id)) return 0.6;\n"
   "    return 0.2;\n"
   "  });\n"
   "  document.getElementById('search-results').innerHTML = 'Found ' + matches.length + ' node(s)';\n"
   "});\n"
   "</script>\n"
   "</body>\n"
   "</html>\n"))

(defun howm-graph--generate-html ()
  "Generate HTML file with graph."
  (let* ((graph-data (howm-graph--parse-notes))
         (json-str (let ((json-encoding-pretty-print nil)) (json-encode graph-data)))
         (d3-script (howm-graph--fetch-d3))
         (html (howm-graph--make-html-content)))
    
    (setq html (replace-regexp-in-string "REPLACE_D3_HERE" d3-script html t t))
    (setq html (replace-regexp-in-string "REPLACE_DATA_HERE" json-str html t t))
    
    (with-temp-file howm-graph-output-file
      (insert html))
    
    (message "Generated graph: %d nodes, %d links"
             (length (plist-get graph-data :nodes))
             (length (plist-get graph-data :links)))
    howm-graph-output-file))

;;;###autoload
(defun howm-graph-view ()
  "Show Howm graph."
  (interactive)
  (let ((file (howm-graph--generate-html)))
    (browse-url (concat "file://" file))))

;;;###autoload
(defun howm-graph-refresh ()
  "Refresh graph."
  (interactive)
  (howm-graph--generate-html)
  (message "Refreshed. Reload browser."))

(provide 'howm-graph-view)
;;; howm-graph-view.el ends here
