(require 'dash)
(require 'advent-utils)

(defun day12/node-to-keyword (node-name)
  (intern (concat ":" node-name)))

(defun day12/keyword-to-node-name (node)
  (substring (symbol-name node) 1))

(defun day12/add--connection-leg (connections pair)
  (let ((destinations (advent/get connections (car pair) nil)))
    (advent/put connections (car pair) (cons (cadr pair) destinations))))

(defun day12/debug--print-connections (connections)
  (maphash (lambda (k v)
             (print (format "%s -> %s" k v)))
           connections))

(defun day12/add-connection (connections pair)
  (day12/add--connection-leg connections pair)
  (day12/add--connection-leg connections (reverse pair))
  connections)

(defun day12/parse-connection (connections line)
  (let ((new-connection (-map #'day12/node-to-keyword (split-string line "-"))))
    (day12/add-connection connections new-connection)))

(defun day12/read-nodes (lines)
  (-reduce-from #'day12/parse-connection (advent/table) lines))

(defun day12/str-path (path)
  (apply #'concat (-interpose "," (-map #'day12/keyword-to-node-name path))))

(defun day12/is-big-cave? (node)
  (let ((name (day12/keyword-to-node-name node)))
    (string= (upcase name) name)))

(defun day12/can-be-visited-simple? (visited node)
  (or (day12/is-big-cave? node)
      (not (advent/get visited node))))

(defun day12/get--next (connections visit-logic state)
  "Returns a list of possible follow-up for the current path, or nil if there is none"
  (let ((visited (plist-get state :visited))
        (current-path (plist-get state :path)))
   (let ((last (car current-path)))
     (unless (eq last :end)
       (--filter (funcall visit-logic visited it) (advent/get connections last))))))

(defun day12/get-updated-visit-count (old-visited node)
  (let ((visited (advent/copy-table old-visited)))
    (advent/put visited node (1+ (advent/get visited node 0)))
    visited))

(defun day12/add--leg (connections visit-logic state)
  "Take a connection and returns a list of expanded paths (or nil) if none could be found"
  (let ((visited (plist-get state :visited))
        (current-path (plist-get state :path)))
   (let ((next-candidates (day12/get--next visit-logic  connections state)))
     (--map (list
             :path (cons it current-path)
             :visited (day12/get-updated-visit-count visited it))
            next-candidates))))

(defun day12/is-complete? (path)
  (and (eq (car path ) :end)
       (eq (car (last path)) :start)))

(defun day12/recurse--paths (connections visit-logic states)
  "evolve the states until there is nothing to evolve"
  ;; There is a -dash function for this…
  (let ((complete-states (--filter (day12/is-complete? (plist-get it :path)) states))
        (incomplete-states (--filter (not (day12/is-complete? (plist-get it :path))) states)))
    (if (not incomplete-states)
        ;; nothing to do, just return the list of complete states
        complete-states
      ;; the next states can be evolved, maybe?
      (let ((updated-states (--map (day12/add--leg visit-logic connections it) incomplete-states)))
        (day12/recurse--paths connections visit-logic (append (apply #'append complete-states updated-states)))))))

(defun day12/compute-all-paths-simple (connections)
  (let ((visited (day12/get-updated-visit-count (advent/table) :start)))
    (--map (reverse (plist-get it :path))
           (day12/recurse--paths connections #'day12/can-be-visited-simple?
                                    (list (list :visited visited :path '(:start))))))  )

(defun day12/part-1 (lines)
  (length
   (day12/compute-all-paths-simple
    (day12/read-nodes lines))))

(defun day12/single-small-cave-already-visited-twice? (visited)
  (let ((visited-twice))
    (maphash (lambda (k v)
               (unless (or (eq k :start)
                       (eq k :end)
                       (day12/is-big-cave? k)
                       (< v 2))
                   (setq visited-twice t)))
             visited)
    visited-twice))

(defun day12/can-be-visited-count? (visited node)
  (unless (eq node :start)
   (or (eq node :end)
       (day12/is-big-cave? node)
       (let ((visit-count (advent/get visited node 0)))
         (< visit-count (if (day12/single-small-cave-already-visited-twice? visited)
                            1
                          2))))))

(defun day12/compute-all-paths-count (connections)
(let ((visited (advent/table)))
    (--map (reverse (plist-get it :path))
           (day12/recurse--paths connections #'day12/can-be-visited-count?
                                    (list (list :visited visited :path '(:start))))))  )

(defun day12/part-2 (lines)
    (length
   (day12/compute-all-paths-count
    (day12/read-nodes lines))))

(provide 'day12)

(defvar example (day12/read-nodes (advent/read-problem-lines 12 :example)))
(defvar tiny (day12/read-nodes (split-string "start-A
start-b
A-c
A-b
b-d
A-end
b-end")))

