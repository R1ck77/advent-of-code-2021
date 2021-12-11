(require 'dash)
(require 'advent-utils)

(defun day11/convert-cell (string-power)
  (vector (string-to-number string-power) nil))

(defun day11/convert-line (line)
  (apply #'vector (-map #'day11/convert-cell (split-string line "" t))))

(defmacro day11/loop-grid (grid &rest forms)
  "Non-hygienic macro that bind all coordinates of the grid to 'it'"
  (declare (indent 1))
  (let ((rows (make-symbol "rows"))
        (columns (make-symbol "columns"))
        (i (make-symbol "i"))
        (j (make-symbol "j"))
        (it (make-symbol "it")))
   `(let ((,rows (length ,grid))
          (,columns (length (aref ,grid 0))))
      (loop for ,i from 0 below ,rows do
            (loop for ,j from 0 below ,columns do
                  (let ((it (cons ,i ,j)))
                    ,@forms))))))

(defun day11/read-grid (lines)
  (apply #'vector
         (-map #'day11/convert-line
               lines)))

(defun day11/str-grid (grid)
  (let ((result ""))
    (loop for i from 0 below (length grid) do
          (let ((row (aref grid i)))
            (loop for j from 0 below (length row) do
                  (let ((value (number-to-string (aref (aref row j) 0))))
                    (setq result (concat result value))))
            (setq result (concat result "\n"))))
    (string-trim result)))

(defun day11/increase-grid-energy-levels! (grid)
  (day11/loop-grid grid
    (let ((cell (aref (aref grid (car it)) (cdr it))))
      (aset cell 0 (1+ (aref cell 0)))))
  grid)

(defun day11/get-cell (grid coord)
  (aref (aref grid (car coord)) (cdr coord)))

(defconst day11/neighbors '((-1 . -1) (-1 . 0) (-1 . 1)
                            (0 . -1) (0 . 1)
                            (1 . -1) (1 . 0) (1 . 1)))

(defun day11/updated-coordinate (grid-size coord displacement)
  (let ((new-row (+ (car coord)
                    (car displacement))))
    (if (and (>= new-row 0) (< new-row (car grid-size)))
        (let ((new-column (+ (cdr coord)
                             (cdr displacement))))
          (and (>= new-column 0) (< new-column (cdr grid-size))
               (cons new-row new-column))))))

(defun day11/get-grid-size (grid)
  (cons (length grid) (length (aref grid 0))))

(defmacro day11/loop-neighbors (grid coord &rest body)
  "Loop over all valid cell neighbors binding the coordinates to 'it'"
  (declare (indent 2))
  (let ((displacement (make-symbol "displacement"))
        (grid-size (make-symbol "grid-size")))
    `(let ((,grid-size (day11/get-grid-size ,grid)))
       (-map (lambda (,displacement)
               (let ((it (day11/updated-coordinate ,grid-size ,coord ,displacement)))
                 (when it                     
                   ,@body)))
             day11/neighbors)
       nil)))

(defun day11/flash--increase! (grid coord)
  "Increase (and possibly flash) all neighbors of coord"
  (day11/loop-neighbors grid coord
    (let ((current-cell (day11/get-cell grid it)))
      ;; Increase the cell value regardless
      (aset current-cell 0 (1+ (aref current-cell 0)))
      ;; recur
      (day11/flash--cell! grid it))))

(defun day11/flash--cell! (grid coord)
  (let ((cell (day11/get-cell grid coord)))
    ;; if the cell didn't flash or the level is 9 or lower
    (unless (or (aref cell 1)
                (< (aref cell 0) 10))
      ;; mark the cell as "flashed"
      (aset cell 1 t)
      ;; flash-increase all neighbors
      (day11/flash--increase! grid coord))))

(defun day11/flash--cycle! (grid)
  "Make some octopi flash. Returns the modified grid"  
  (day11/loop-grid grid
    (day11/flash--cell! grid it)))

(defun day11/clear-count-flashes! (grid)
  (let ((counter 0))
    (day11/loop-grid grid
      (let ((cell (day11/get-cell grid it)))
        (when (aref cell 1)
          (setq counter (1+ counter))
          (aset cell 1 nil)
          (aset cell 0 0))
        ;; make sure that at the end there are no unflashed cells with more than 9
        (assert (< (aref cell 0) 10))))
    counter))

(defun day11/evolve! (grid)
  "Evolve the grid and returns the number of flashes"
  (day11/increase-grid-energy-levels! grid)
  (day11/flash--cycle! grid)
  (day11/clear-count-flashes! grid))

(defun day11/part-1 (lines)
  (let ((grid (day11/read-grid lines))
        (counter 0))
    (loop repeat 100 do
          (setq counter (+ counter (day11/evolve! grid))))
    counter))

(defun day11/total-cells (grid)
  (let ((size (day11/get-grid-size grid)))
    (* (car size) (cdr size))))

(defun day11/part-2 (lines)
  (setq max-lisp-eval-depth 1000000)
  (setq max-specpdl-size 1000000)
  (let ((grid (day11/read-grid lines)))
    (let ((total-cells (day11/total-cells grid))
          (flashes 0)
          (step 0))
      (loop until (= flashes total-cells)
            do
            (progn 
              (setq step (1+ step))
              (setq flashes (day11/evolve! grid))
              step))
      step)))

(provide 'day11)
