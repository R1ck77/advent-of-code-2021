(require 'dash)
(require 'advent-utils)

(defun day15/get-distance (grid coord)
  (plist-get (advent/grid-get grid coord) :distance))

(defun day15/set-distance (grid coord new-distance)
  (advent/-update-grid-value! grid coord
    (list :value (plist-get :value it)
          :distance new-distance
          :visited (plist-get :visited it))))

(defun day15/pick-next-current (grid unvisited)
  (error "Not implemented")
  (advent/-map-hash unvisited
    
    )
  )

(defun day15/set-visited! (grid coord)
  (advent/-update-grid-value! grid coord
    (list :value (plist-get :value it)
          :distance (plist-get :distance it)
          :visited t)))

(defun day15/remove-current! (grid current unvisited)
  (day15/set-visited! grid current)
  (remhash current unvisited))

(defun day15/update-distance! (grid current other)
  (error "Not implemented"))

(defun day15/valid-coordinate? (coord limit)
  (and
   (>= coord 0)
   (< coord limit)))

(defun day15/grid-size (grid)
  (cons (length grid) (length (aref grid 0))))

(defun day15/get-neighbors (grid coord)
  ;; TODO/FIXME This should be cached…
  (let ((grid-size (day15/grid-size)))
    (--filter (and (day15/valid-coordinate? (car it) (car grid-size))
                   (day15/valid-coordinate? (cdr it) (cdr grid-size)))
              (--map (cons (+ (car coord)
                              (car it))
                           (+ (cdr coord)
                              (cdr it)))
                     '((-1 . 0) (1 . 0) (0 . -1) (0 . 1))))))

(defun day15/get-unvisited-neighbors (grid current unvisited)
  "Returns a list of unvisited neighbors"
  (--filter (not (advent/get unvisited it))
            (day15/get-neighbors grid current)))

(defun day15/set--initial-grid! (grid current)
  "Set all nodes to 'unvisited' (cached), mark the initial distances"
  (let ((grid (advent/copy-grid grid)))    
    (advent/-update-grid! grid
      (list :value it :distance nil :visited nil))
    (advent/-update-grid-value! grid current
      (list :value (plist-get it :value) :distance 0 :visited nil))
    grid))

(defun day15/create--unvisited-set (grid)
  (let ((unvisited (advent/table))
        (grid-size (day15/grid-size grid)))
   (loop for i below (car grid-size) do
         (loop for j below (cdr grid-size) do
               (advent/put unvisited (cons i j) t)))
   unvisited))

(defun day15/dijkstra (grid)
  (let ((grid (day15/set--initial-grid! grid))
        (unvisited (day15/create--unvisited-set grid))
        (current (cons 0 0)))
    (let ((neighbors (day15/get-unvisited-neighbors grid current unvisited)))
      (while current
        (--each neighbors (day15/update-distance! grid current it))
        (day15/remove-current! grid current unvisited)
        (setq current (day15/pick-next-current grid unvisited))))
    grid))

(defun day15/part-1 (grid)
  (let ((dijkstred (day15/dijkstra grid))
        (grid-size (day15/grid-size grid)))
    (plist-get (aref (aref dijkstred (car grid-size)) (cdr grid-size)) :distance)))

(defun day15/part-2 (grid)
  (error "Not yet implemented"))

(provide 'day15)
