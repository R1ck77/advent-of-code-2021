(require 'dash)
(require 'advent-utils)

(defun day13/read-coordinate (line)
  (let ((as-list (-map #'string-to-number
                       (split-string line "," t))))
    (cons (car as-list) (cadr as-list))))

(defun day13/read-points-coordinates (lines)
  (-map #'day13/read-coordinate lines))

(defun day13/create-empty-paper (width-height)  
  (apply #'vector
         (--map (make-vector (car width-height) nil)
                (number-sequence 0 (1- (cdr width-height))))))

(defun day13/read-paper (grid-size lines)
  (let ((grid (day13/create-empty-paper grid-size)))
    (--each (day13/read-points-coordinates lines)
      (let ((row (aref grid (cdr it))))
        (aset row (car it) t)))
    grid))

(defun day13/read-fold (line)
  (let ((tokens (split-string line " " t)))
    (assert (string= (car tokens) "fold"))
    (let ((raw (split-string (elt tokens 2)  "=" t)))
      (if (string= (car raw) "x")
          (cons (string-to-number (cadr raw)) 0)
        (cons 0 (string-to-number (cadr raw)))))))

(defun day13/read-folds (lines)
  (-map #'day13/read-fold lines))

(defun day13/compute-grid-size (folds)
  (let ((first-x (caar (--filter (not (zerop (car it))) folds)))
        (first-y (cdar (--filter (not (zerop (cdr it))) folds))))
    (cons (1+ (* first-x 2))
          (1+ (* first-y 2)))))

(defun day13/read-state (blocks)
  (let ((folds (day13/read-folds (cadr blocks))))
    (cons (day13/read-paper (day13/compute-grid-size folds)
                            (car blocks))
          folds)))

(defun day13/get (state column-row)
  (aref (aref (car state) (cdr column-row)) (car column-row)))

(defun day13/reverse-get (state row-column)
  (day13/get state (cons (cdr row-column) (car row-column))))

(defun day13/cut-paper-y (old-paper y)
  "Cut paper along x on the specified row"
  (assert (= (1+ (* y 2)) (length old-paper)))
  (let ((paper-columns (length (aref old-paper 0))))
    (let ((new-paper (day13/create-empty-paper (cons paper-columns y))))
      (loop for i from 0 below y do
            (let ((old-row (aref old-paper i))
                  (new-row (aref new-paper i)))
              (loop for j from 0 below paper-columns do
                    (aset new-row j (aref old-row j)))))
      new-paper)))

(defun day13/mirror-y! (old-paper new-paper y)
  (let ((paper-columns (length (aref old-paper 0)))
        (old-paper-rows (length old-paper)))
    (loop for i from 0 below y do
          (let ((new-row (aref new-paper i))
                (old-row (aref old-paper (- old-paper-rows i 1))))
            (loop for j from 0 below paper-columns do                
                  (aset new-row j (or (aref new-row j)
                                      (aref old-row j))))))
    new-paper))

(defun day13/fold-y (old-paper coord)
  (let* ((y (cdr coord))
        (new-paper (day13/cut-paper-y old-paper y)))
    (day13/mirror-y! old-paper new-paper y)
    new-paper))

(defun day13/cut-paper-x (old-paper x)
  "Cut paper along y on the specified column"
  (assert (= (1+ (* x 2)) (length (aref old-paper 0))))
  (let ((paper-rows (length old-paper)))
    (let ((new-paper (day13/create-empty-paper (cons x paper-rows))))
      (loop for i from 0 below paper-rows do
            (let ((old-row (aref old-paper i))
                  (new-row (aref new-paper i)))
              (loop for j from 0 below x do
                    (aset new-row j (aref old-row j)))))
      new-paper)))

(defun day13/mirror-x! (old-paper new-paper x)
  (let ((paper-rows (length old-paper))
        (old-paper-columns (length (aref old-paper 0))))
    (loop for i from 0 below paper-rows do
          (let ((new-row (aref new-paper i))
                (old-row (aref old-paper i)))
            (loop for j from 0 below x do                
                  (aset new-row j (or (aref new-row j)
                                      (aref old-row (- old-paper-columns j 1)))))))
    new-paper))

(defun day13/fold-x (old-paper coord)
  (let* ((x (car coord))
        (new-paper (day13/cut-paper-x old-paper x)))
    (day13/mirror-x! old-paper new-paper x)
    new-paper))

(defun day13/fold (state)
  (let ((paper (car  state))
        (folds (cdr state)))
    (let ((current-fold (car folds)))      
      (cons (funcall (if (zerop (car current-fold))
                         #'day13/fold-y
                       #'day13/fold-x)
                     paper
                     current-fold)
            (cdr folds)))))

(defun day13/fold-all (state)
  (--reduce-from (day13/fold acc) state (cdr state)))

(defun day13/format-paper (state)
  (let ((result "")
        (paper (car state)))
    (loop for i
          from 0
          below (length paper)
          do
          (loop for j
                from 0
                below (length (aref paper 0))
                do
                (setq result (concat result (if (day13/get state (cons j i)) "#" ".")))
                )
          (setq result (concat result "\n")))
    result))

(defun day13/count-marks (state)
  (let ((counter 0))
    (advent/loop-grid (car state)
      (let ((row-column it))
        (when (day13/reverse-get state it)
          (setq counter (1+ counter)))))
    counter))

(defun day13/part-1 (lines)
  (day13/count-marks (day13/fold (day13/read-state lines))))

(defun day13/part-2 (lines)
  (day13/format-paper (day13/fold-all (day13/read-state lines))))

(provide 'day13)
