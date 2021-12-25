(require 'dash)
(require 'advent-utils)

(defconst day25/debug-buffer "* Day 25 *")

(defun day25/symbol-to-string (symbol)
  (or (case symbol
     (:> ">")
     (:v "v"))
      "."))

(defun day25/to-string (grid)
  (let ((result ""))
    (advent/-each-grid grid
      (when (and (zerop (cdr it-coord))
               (not (equal it-coord '(0 . 0))))
        (setq result (concat result "\n")))
      (setq result (concat result (day25/symbol-to-string it-value))))
    result))

(defun day25/debug--print (grid)
  (with-current-buffer (get-buffer-create day25/debug-buffer)
    (erase-buffer)
    (goto-char (point-min)) ;overkill probably
    (insert (day25/to-string grid))))

(defun day25/read-map (grid)
  (advent/-update-grid! grid
    (cond
     ((string= it ">") :>)
     ((string= it "v") :v)
     ((string= it ".") nil)
     ((t (error "Unexpected value!"))))))

(defun day25/read-column (grid column rows)
  (let ((new-column (make-vector rows nil)))
    (loop for i from 0 below rows do
          (aset new-column i (aref (aref grid i) column)))
    new-column))

(defun day25/write-column! (grid column vector rows)
  (loop for i from 0 below rows do
        (aset (aref grid i) column (aref vector i))))

(defun day25/evolve-line! (vector n type)
  (let ((i 0)
        (copy (copy-sequence vector)))
    (while (< i n)
      (let ((here (aref copy i)))
        (if (eq here type)
            (let* ((next-coord (mod (1+ i) n))
                   (next (aref copy next-coord)))
              (when (not next)
                (aset vector i nil)
                (aset vector next-coord type)
                (setq i (1+ i)))))
        (setq i (1+ i)))))
  )

(defun day25/evolve-column! (vector rows)
  (day25/evolve-line! vector rows :v))

(defun day25/evolve-row! (vector columns)
  (day25/evolve-line! vector columns :>))

(defun day25/evolve (grid)
    (let* ((grid-size (advent/get-grid-size grid))
           (new-grid (advent/copy-grid grid)))
      (loop for row from 0 below (car grid-size) do
            (day25/evolve-row! (aref new-grid row) (cdr grid-size)))
      (loop for column from 0 below (cdr grid-size) do
            (let ((column-vector (day25/read-column new-grid column (car grid-size))))
              (day25/evolve-column! column-vector (car grid-size))
              (day25/write-column! new-grid column column-vector (car grid-size))))
      new-grid))

(defun day25/evolve-until-stable (grid)
  (let ((next (day25/evolve grid))
        (counter 1))
    (while (not (equal next grid))
      (setq grid next)
      (setq next (day25/evolve grid))
      (setq counter (1+ counter)))
    counter))

(defun day25/test--evolve-n (grid n)
  (--reduce-from (day25/evolve acc) grid (number-sequence 1 n)))

(defun day25/part-1 (lines)
  (day25/evolve-until-stable (day25/read-map lines)))

(defun day25/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day25)

(setq example (day25/read-map (advent/read-grid 25 :example #'identity)))
(setq problem (day25/read-map (advent/read-grid 25 :problem #'identity)))
