(require 'dash)
(require 'advent-utils)

(defun day15/set--initial-grid! (grid current)
  "Set all nodes to 'unvisited' (cached), mark the initial distances"
  (let ((grid (advent/copy-grid grid)))    
    (advent/-update-grid! grid
      (list :value it :distance nil :visited nil))
    (advent/-update-grid-value! grid current
      (list :value (plist-get it :value) :distance 0 :visited nil))
    grid))

(defun day15/create--unvisited-set (grid)
  (let ((unvisited (advent/table)))
   (loop for i below (length grid) do
         (loop for j below (length (aref grid 0)) do
               (advent/put unvisited (cons i j) t)))
   unvisited))

(defun day15/dijkstra (grid)
  (let ((grid (advent/copy-grid grid))
        (unvisited (day15/create--unvisited-set grid)))
    (day15/set--initial-distances! grid '(0 . 0))))

(defun day15/part-1 (grid)
  (error "Not yet implemented"))

(defun day15/part-2 (grid)
  (error "Not yet implemented"))

(provide 'day15)
