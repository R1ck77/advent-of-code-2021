(require 'dash)
(require 'advent-utils)

(defun day9/read-line (line)
  (apply #'vector (-map #'string-to-number (split-string line "" t))))

(defun day9/read-map (lines)
  (apply #'vector (-map #'day9/read-line lines)))

(defun day9/get (map row-column)
  (condition-case nil
      (aref (aref map (car row-column)) (cdr row-column))
    (error nil)))

(defun day9/get-neighbors (map row-column)
  "Return a list with: center value, N, E, S,W values, possibly nil"
  (let ((row (car row-column))
        (column (cdr row-column)))
   (list (day9/get map row-column)
         (day9/get map (cons (1- row) column))
         (day9/get map (cons row (1+ column)))
         (day9/get map (cons (1+ row) column))
         (day9/get map (cons row (1- column))))))

(defun day9/is-minimum? (map row-column)
  "Return the value at position if it's a minimum, or nil otherwise"
  (let* ((neighbors (day9/get-neighbors map row-column))
        (center (car neighbors)))
    (unless (--any (<= it center) (-non-nil (rest neighbors)))
      center)))

(defun day9/get-map-size (map)
  (list (length map)
        (length (elt map 0))))

(defun day9/get-map-coordinates (map)
  (apply #'advent/create-coordinates
         (day9/get-map-size map)))

(defun day9/sum-minima-risk (map)
  (let ((size ))
    (apply #'+ (-map #'1+
           (-non-nil
            (--map (day9/is-minimum? map it)
                   (day9/get-map-coordinates map)))))))

(defun day9/part-1 (lines)
  (day9/sum-minima-risk (day9/read-map lines)))

(defun day9/get-around-coordinates (coordinate)
  (let ((row (car coordinate))
        (column (cdr coordinate)))
   (list (cons (1- row) column)
         (cons row (1+ column))
         (cons (1+ row) column)
         (cons row (1- column)))))

(defun day9/get-valid-neighbors (map coordinate used-coords)
  (-map #'car
        (--filter (not (advent/get used-coords (car it))) ; don't count coordinates already used
                  (--filter (/= (cadr it) 9)              ; remove 9s
                            (--filter (cadr it) ; remove out-of map coodinates
                                      (--map (list it (day9/get map it))
                                             (day9/get-around-coordinates coordinate)))))))

(defun day9/get-current-basin (map coordinate used-coords)
  ;; Mark the current coordinate as "used"
  (unless (advent/get used-coords coordinate)
    (advent/put used-coords coordinate t)
    (let ((valid-neighbors (day9/get-valid-neighbors map coordinate used-coords)))
      (--reduce-from (append acc (day9/get-current-basin map it used-coords)) (list coordinate) valid-neighbors))))

(defun day9/find-basin (acc coordinate)
  (let ((map (elt acc 0))
        (used-coords (elt acc 1))
        (basins-list (elt acc 2)))
    (cond
     ;; already used
     ((advent/get used-coords coordinate) acc)
     ;; not interesting
     ((= (day9/get map coordinate) 9) acc) 
     ;; new basin
     (t (list map used-coords (cons (day9/get-current-basin map coordinate used-coords) basins-list))))))

(defun day9/get-basins (map)
  (elt (-reduce-from #'day9/find-basin
                 (list map (advent/table) nil)
                 (day9/get-map-coordinates map))
       2))

(defun day9/part-2 (lines)
  (let* ((map (day9/read-map lines))
         (basins (day9/get-basins map)))
    (apply #'*
           (-take 3
                  (-sort #'> (-map #'length basins))))))

(provide 'day9)
(setq max-lisp-eval-depth 100000)
(setq max-specpdl-size 100000)
