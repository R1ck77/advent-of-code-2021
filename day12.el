(require 'dash)
(require 'advent-utils)

(defun day12/node-to-keyword (node-name)
  (intern (concat ":" node-name)))

(defun day12/keyword-to-node (node)
  (substring (symbol-name node) 1))

(defun day12/add--leg (connections pair)
  (let ((destinations (advent/get connections (car pair) nil)))
    (advent/put connections (car pair) (cons (cadr pair) destinations))))

(defun day12/add-connection (connections pair)
  (day12/add--leg connections pair)
  (day12/add--leg connections (reverse pair))
  connections)

(defun day12/parse-connection (connections line)
  (let ((new-connection (-map #'day12/node-to-keyword (split-string line "-"))))
    (day12/add-connection (or connections (advent/table)) new-connection)))

(defun day12/read-nodes (lines)
  (-reduce-from #'day12/parse-connection nil lines))

(defun day12/str-path (path)
  (apply #'concat (-interpose "," (-map #'day12/keyword-to-node path))))

(defun day12/compute-all-paths (connections)
  nil)

(defun day12/part-1 (lines)
  (length
   (day12/compute-all-paths
    (day12/read-nodes lines))))

(defun day12/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day12)
