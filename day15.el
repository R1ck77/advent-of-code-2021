(require 'dash)
(require 'advent-utils)

;; :value :distance :visited

(defconst value-idx 0)
(defconst distance-idx 1)
(defconst visited-idx 2)

(defun day15/get--property (grid coord property-idx)
  (aref (advent/grid-get grid coord) property-idx))

(defun day15/get-value (grid coord)
  (day15/get--property grid coord value-idx))

(defun day15/get-distance (grid coord)
  (day15/get--property grid coord distance-idx))

(defun day15/set-distance (grid coord new-distance)
  (advent/-update-grid-value! grid coord
    (aset it 1 new-distance)
    it))

(defun day15/pick-from-distance (grid unvisited neighbors with-distance)
  (when (> (hash-table-count unvisited) (hash-table-count with-distance))
    (caar
     (sort (--filter (not (aref (cdr it) visited-idx))
                     (advent/-map-hash with-distance (cons it-key it-value)))
           (lambda (a b)
             (< (aref (cdr a) distance-idx)
                (aref (cdr b) distance-idx)))))))

(defun day15/pick-next-current (grid unvisited neighbors with-distance)
  ;; Check the direct neighbors first. Should help…
  (or (day15/pick-from-distance grid unvisited neighbors with-distance)
   (caar
    (sort (--filter (cdr it)
                    (advent/-map-hash unvisited (cons it-key (aref it-value distance-idx))))
          (lambda (a b)
            (< (cdr a)
               (cdr b)))))))

(defun day15/pick-next-current (grid unvisited neighbors with-distance)
  ;; Check the direct neighbors first. Should help…
  (or (day15/pick-from-distance grid unvisited neighbors with-distance)
   (caar
    (sort (--filter (cdr it)
                    (--map (cons it (aref (advent/grid-get grid it) distance-idx))
                           (advent/-map-hash unvisited it-key)))
          (lambda (a b)
            (< (cdr a)
               (cdr b)))))))

(defun day15/set-visited! (grid coord)
  (advent/-update-grid-value! grid coord
    (aset it visited-idx t)
    it))

(defun day15/remove-current! (grid current unvisited with-distance)
  (day15/set-visited! grid current)
  (remhash current unvisited)
  (remhash current with-distance))

(defun day15/update-distance! (grid current other with-distance)
  (let* ((other-value (advent/grid-get grid other))
        (computed-distance (+ (day15/get-distance grid current)
                              (aref other-value value-idx)))
        (previous-distance (aref other-value distance-idx)))
    ;;; Optimization opportunity, also bug
    (day15/set-distance grid other (min (or previous-distance computed-distance)
                                        computed-distance))    
    (advent/put with-distance other other-value)))

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
                     '((-1 . 0) (1 . 0) (0 . -1) (0 . 1))))))

(defun day15/get-unvisited-neighbors (grid current unvisited)
  "Returns a list of unvisited neighbors"
  (--filter (advent/get unvisited it)
            (day15/get-neighbors grid current)))

(defun day15/set--initial-grid! (grid current)
  "Set all nodes to 'unvisited' (cached), mark the initial distances"
  (let ((grid (advent/copy-grid grid)))    
    (advent/-update-grid! grid (apply #'vector (list it nil nil)))
    (advent/-update-grid-value! grid current (apply #'vector (list (aref it value-idx) 0 nil)))
    grid))

(defun day15/create--unvisited-set (grid)
  (let ((unvisited (advent/table))
        (grid-size (day15/grid-size grid)))
   (loop for i below (car grid-size) do
         (loop for j below (cdr grid-size) do
               (let ((unvisited-coord (cons i j)))
                (advent/put unvisited unvisited-coord (advent/grid-get grid unvisited-coord)))))
   unvisited))

(defun day15/debug--grid (grid property)
  (print (format "Debugging %s for the grid" property))
    (print (advent/debug-str-grid grid "%10s"))
    (comment (print (advent/debug-str-grid (advent/-update-grid! (advent/copy-grid grid)
                                     (plist-get it property))
                                   "%3s"))))

(defun day15/dijkstra (grid)
  (let* ((current (cons 0 0))
         (grid (day15/set--initial-grid! grid current))
         (unvisited (day15/create--unvisited-set grid))
         (with-distance (advent/table)))
    (advent/reset-steps)
    (while current
      (let ((neighbors (day15/get-unvisited-neighbors grid current unvisited)))
        
        
        (assert (day15/get-value grid current))
        (--each neighbors (day15/update-distance! grid current it with-distance))
        (day15/remove-current! grid current unvisited with-distance)
        (setq current (day15/pick-next-current grid unvisited neighbors with-distance)))
      (comment (day15/debug--grid grid distance-idx))
      (advent/step))
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
