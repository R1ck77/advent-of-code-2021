(require 'dash)
(require 'advent-utils)

(defun day15/get--property (grid coord property)
  (plist-get (advent/grid-get grid coord) property))

(defun day15/get-value (grid coord)
  (day15/get--property grid coord :value))

(defun day15/get-distance (grid coord)
  (day15/get--property grid coord :distance))

(defun day15/set-distance (grid coord new-distance)
  (advent/-update-grid-value! grid coord
    (list :value (plist-get it :value)
          :distance new-distance
          :visited (plist-get it :visited))))

(defun day15/pick-next-current (grid unvisited)
  (car
   (--filter (let ((value (advent/grid-get grid it)))
               (assert (not (plist-get value :visited)))
               (plist-get value :distance))
             (advent/-map-hash unvisited it-key))))

(defun day15/set-visited! (grid coord)
  (advent/-update-grid-value! grid coord
    (list :value (plist-get it :value)
          :distance (plist-get it :distance)
          :visited t)))

(defun day15/remove-current! (grid current unvisited)
  (day15/set-visited! grid current)
  (remhash current unvisited))

(defun day15/update-distance! (grid current other)
  (let ((computed-distance (+ (day15/get-distance grid current)
                              (day15/get-value grid other)))
        (previous-distance (day15/get-distance grid other)))
    (day15/set-distance grid other (min (or previous-distance computed-distance)
                                        computed-distance))))

(defun day15/valid-coordinate? (coord limit)
  (and
   (>= coord 0)
   (< coord limit)))

(defun day15/grid-size (grid)
  (cons (length grid) (length (aref grid 0))))

(defun day15/get-neighbors (grid coord)
  ;; TODO/FIXME This should be cached…
  (let ((grid-size (day15/grid-size grid)))
    (--filter (and (day15/valid-coordinate? (car it) (car grid-size))
                   (day15/valid-coordinate? (cdr it) (cdr grid-size)))
              (--map (cons (+ (car coord)
                              (car it))
                           (+ (cdr coord)
                              (cdr it)))
                     ;'((-1 . 0) (1 . 0) (0 . -1) (0 . 1))
                     '((1 . 0) (0 . 1))
                     ))))

(defun day15/get-unvisited-neighbors (grid current unvisited)
  "Returns a list of unvisited neighbors"
  (--filter (advent/get unvisited it)
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

(defun day15/debug--grid (grid property)
  (print (format "Debugging %s for the grid" property))
  (print (advent/debug-str-grid (advent/-update-grid! (advent/copy-grid grid)
                                  (plist-get it property)))))

(defun day15/dijkstra (grid)
  (let* ((current (cons 0 0))
         (grid (day15/set--initial-grid! grid current))
         (unvisited (day15/create--unvisited-set grid)))
    (while current
      (let ((neighbors (day15/get-unvisited-neighbors grid current unvisited)))
        
        
        (assert (day15/get-value grid current))
        (--each neighbors (day15/update-distance! grid current it))
        (day15/remove-current! grid current unvisited)
        (setq current (day15/pick-next-current grid unvisited)))
      ;(day15/debug--grid grid :distance)
      )
    grid))

(defun day15/solve-grid (grid)
  (let ((dijkstred (day15/dijkstra grid))
        (grid-size (day15/grid-size grid)))
    (day15/get-distance dijkstred (cons (1- (car grid-size))
                                        (1- (cdr grid-size))))))

(defun day15/part-1 (grid)
  (day15/solve-grid grid))

(defun day15/increment-value (increment old-value)
    (1+ (mod (+ (1- old-value) increment) 9)))

(defun day15/copy-increased (grid dest increment start-row start-column)
  (let ((grid-size (day15/grid-size grid)))
    (loop for i below (car grid-size) do
          (loop for j below (cdr grid-size) do
                (let ((original-value (advent/grid-get grid (cons i j))))
                  (advent/grid-set! dest
                                    (cons (+ start-row i)
                                          (+ start-column j))
                                    (day15/increment-value increment original-value)))))))

(defun day15/multiply-grid (grid)
  (let* ((grid-size (day15/grid-size grid))
        (new-grid (advent/make-grid (* 5 (car grid-size))
                                    (* 5 (cdr grid-size))
                                    nil)))
    (loop for row-r below 5 do
          (loop for column-r below 5 do
                (let ((increment (+ row-r column-r)))
                  (day15/copy-increased grid
                                        new-grid
                                        increment
                                        (* (car grid-size) row-r)
                                        (* (cdr grid-size) column-r)))))
    new-grid))

(defun day15/part-2 (grid)
  (day15/solve-grid (day15/multiply-grid grid)))

(provide 'day15)

(defvar example (advent/read-grid 15 :example))
(defvar problem (advent/read-grid 15 :problem))
