(require 'dash)
(require 'advent-utils)

(defun day09.el/read-line (line)
  (apply #'vector (-map #'string-to-number (split-string line "" t))))

(defun day09.el/read-map (lines)
  (apply #'vector (-map #'day09.el/read-line lines)))

(defun day09.el/get (map row-column)
  (condition-case nil
      (aref (aref map (car row-column)) (cdr row-column))
    (error nil)))

(defun day09.el/get-neighbors (map row-column)
  "Return a list with: center value, N, E, S,W values, possibly nil"
  (let ((row (car row-column))
        (column (cdr row-column)))
   (list (day09.el/get map row-column)
         (day09.el/get map (cons (1- row) column))
         (day09.el/get map (cons row (1+ column)))
         (day09.el/get map (cons (1+ row) column))
         (day09.el/get map (cons row (1- column))))))

(defun day09.el/is-minimum? (map row-column)
  "Return the value at position if it's a minimum, or nil otherwise"
  (let* ((neighbors (day09.el/get-neighbors map row-column))
        (center (car neighbors)))
    (unless (--any (<= it center) (-non-nil (rest neighbors)))
      center)))

(defun day09.el/get-map-size (map)
  (list (length map)
        (length (elt map 0))))

(defun day09.el/get-map-coordinates (map)
  (apply #'advent/create-coordinates
         (day09.el/get-map-size map)))

(defun day09.el/sum-minima-risk (map)
  (let ((size ))
    (apply #'+ (-map #'1+
           (-non-nil
            (--map (day09.el/is-minimum? map it)
                   (day09.el/get-map-coordinates map)))))))

(defun day09.el/part-1 (lines)
  (day09.el/sum-minima-risk (day09.el/read-map lines)))

(defun day09.el/get-around-coordinates (coordinate)
  (let ((row (car coordinate))
        (column (cdr coordinate)))
   (list (cons (1- row) column)
         (cons row (1+ column))
         (cons (1+ row) column)
         (cons row (1- column)))))

(defun day09.el/get-valid-neighbors (map coordinate used-coords)
  (-map #'car
        (--filter (not (advent/get used-coords (car it))) ; don't count coordinates already used
                  (--filter (/= (cadr it) 9)              ; remove 9s
                            (--filter (cadr it) ; remove out-of map coodinates
                                      (--map (list it (day09.el/get map it))
                                             (day09.el/get-around-coordinates coordinate)))))))

(defun day09.el/get-current-basin (map coordinate used-coords)
  ;; Mark the current coordinate as "used"
  (unless (advent/get used-coords coordinate)
    (advent/put used-coords coordinate t)
    (let ((valid-neighbors (day09.el/get-valid-neighbors map coordinate used-coords)))
      (--reduce-from (append acc (day09.el/get-current-basin map it used-coords)) (list coordinate) valid-neighbors))))

(defun day09.el/find-basin (acc coordinate)
  (let ((map (elt acc 0))
        (used-coords (elt acc 1))
        (basins-list (elt acc 2)))
    (cond
     ;; already used
     ((advent/get used-coords coordinate) acc)
     ;; not interesting
     ((= (day09.el/get map coordinate) 9) acc) 
     ;; new basin
     (t (list map used-coords (cons (day09.el/get-current-basin map coordinate used-coords) basins-list))))))

(defun day09.el/get-basins (map)
  (elt (-reduce-from #'day09.el/find-basin
                 (list map (advent/table) nil)
                 (day09.el/get-map-coordinates map))
       2))

(defun day09.el/part-2 (lines)
  (setq max-lisp-eval-depth 10000)
  (setq max-specpdl-size 10000)
  
  (let* ((map (day09.el/read-map lines))
         (basins (day09.el/get-basins map)))
    (apply #'*
           (-take 3
                  (-sort #'> (-map #'length basins))))))

(provide 'day09.el)
