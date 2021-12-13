(require 'dash)
(require 'advent-utils)

(defun day13/read-coordinate (line)
  (let ((as-list (-map #'string-to-number
                       (split-string line "," t))))
    (cons (car as-list) (cadr as-list))))

(defun day13/read-points-coordinates (lines)
  (-map #'day13/read-coordinate lines))

(defun day13/compute-grid-size (points)
  (let ((further-point (--reduce-from (cons (max (car it) (car acc))
                              (max (cdr it) (cdr acc)))
                        '(0 . 0)
                        points )))
    (cons (1+ (car further-point))
          (1+ (cdr further-point)))))

(defun day13/create-empty-paper (width-height)  
  (apply #'vector
         (--map (make-vector (car width-height) nil)
                (number-sequence 0 (1- (cdr width-height))))))

(defun day13/read-paper (lines)
  (let ((points (day13/read-points-coordinates lines)))
    (let ((grid (day13/create-empty-paper (day13/compute-grid-size points))))
      (--each points
        (let ((row (aref grid (cdr it))))
          (aset row (car it) t)))
      grid)))

(defun day13/read-fold (line)
  (let ((tokens (split-string line " " t)))
    (assert (string= (car tokens) "fold"))
    (let ((raw (split-string (elt tokens 2)  "=" t)))
      (if (string= (car raw) "x")
          (cons (string-to-number (cadr raw)) 0)
        (cons 0 (string-to-number (cadr raw)))))))

(defun day13/read-folds (lines)
  (-map #'day13/read-fold lines))

(defun day13/read-state (blocks)
  (cons (day13/read-paper (car blocks))
        (day13/read-folds (cadr blocks))))

(defun day13/get (state column-row)
  (aref (aref (car state) (cdr column-row)) (car column-row)))

(defun day13/reverse-get (state row-column)
  (day13/get state (cons (cdr row-column) (car row-column))))

(defun day13/fold (state)
  state)

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
  (error "Not yet implemented"))

(defun day13/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day13)
