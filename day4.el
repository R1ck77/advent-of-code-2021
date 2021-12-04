(require 'dash)
(require 'advent-utils)

(defun day4/read-numbers (line)
  (-map #'string-to-number (split-string line "," t)))

(defun day4/read-table (lblock)
  (--map (-map #'string-to-number (split-string it " " t)) lblock))

(defun day4/read-problem (blocks)
  (let ((numbers (day4/read-numbers (caar blocks)))
        (tables (-map #'day4/read-table (cdr blocks))))
    (list numbers tables)))

(defun day4/transpose (lines)
  (loop for column from 0 below (length lines)
        collect (loop for line in lines
                      collect (elt line column))))

(defun day4/index-block (numbers lines)
  "Index a block using the list of extractions

Return the minimum number of extractions required to complete a
line/column in a board, the last extracted number and the list of
unmarked values.

If the board cannot be completed with the extractions, return the
number of extractions, the last value extracted, and the
remaining values."
  (let ((rows lines)
        (columns (day4/transpose lines))
        (number nil)
        (extractions 0))
    (while (and numbers
                (not (-any #'null rows))
                (not (-any #'null columns)))
      (setq number (car numbers))
      (setq extractions (1+ extractions))
      (setq rows (--map (remove number it) rows))
      (setq columns (--map (remove number it) columns))
      (setq numbers (cdr numbers)))
    (list extractions number rows)))

(defun day4/index-blocks (problem)
  (let ((extractions (car problem))
        (tables (cadr problem)))
    (--map (day4/index-block extractions it) tables)))

(defun day4/get-indexed-solutions (blocks ordering-f)
  (car
   (--sort (funcall ordering-f
                    (elt it 0)
                    (elt other 0))
           (day4/index-blocks (day4/read-problem blocks)))))

(defun day4/sum-remaining-values (rows)
  "Given a list of rows (possibly null) return the sum of all values"
  (apply #' + (--map (apply #'+ it) rows)))

(defun day4/compute-score (indexed-solution)
  (* (elt indexed-solution 1)
     (day4/sum-remaining-values (elt indexed-solution 2))))

(defun day4/part-1 (blocks)
  (day4/compute-score
   (day4/get-indexed-solutions blocks #'<)))

(defun day4/part-2 (blocks)
  (day4/compute-score
   (day4/get-indexed-solutions blocks #'>)))

(provide 'day4)
