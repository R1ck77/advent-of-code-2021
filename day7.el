(require 'dash)
(require 'advent-utils)

;;; extract
(defun day7/read-crabs (line)
  (-map #'string-to-number (split-string line "," t)))

(defun day7/compute-displacement-cost (bins destination cost-formula)
  (let ((cost 0))
    (maphash (lambda (pos count)
               (setq cost (+ cost(* count (funcall cost-formula pos destination)))))
             bins)
    cost))

(defun day7/bin-crabs (crabs)
  "Save the number of crabs at each position in a table"
  (let ((bins (advent/table)))
    (--each crabs (advent/put bins it (1+ (advent/get bins it 0))))
    bins))

(defun day7/compute-all-costs (crabs cost-formula)
  "It naively computes the cost by adding the cost of each crab.

A smarter approach would be to bin the crabs first, an even
smarter one would be to figure the cost-formula out…"
  (let ((min-pos (apply #'min crabs))
        (max-pos (apply #'max crabs))
        (binned-crabs (day7/bin-crabs crabs)))
    (--map (cons it (day7/compute-displacement-cost binned-crabs it cost-formula))
           (number-sequence min-pos max-pos))))

(defun day7/compare--costs (cons-a cons-b)
  "cons-a and cons-b are cons of (index . cost) "
  (< (cdr cons-a)
     (cdr cons-b)))

(defun day7/simple-displacement-cost (p1 p2)
  (abs (- p1 p2)))

(defun day7/compute-most-efficient-displacement-cost (crabs cost-formula)
  (cdar
   (-sort #'day7/compare--costs
          (day7/compute-all-costs (day7/read-crabs line)
                                  cost-formula))))

(defun day7/part-1 (line)
  (day7/compute-most-efficient-displacement-cost (day7/read-crabs line)
                                                 #'day7/simple-displacement-cost))

(defun day7/advanced-displacement-cost (p1 p2)
  (let ((tmp (abs (- p1 p2))))
    (/ (* tmp (1+ tmp)) 2)))

(defun day7/part-2 (line)
  (day7/compute-most-efficient-displacement-cost (day7/read-crabs line)
                                                 #'day7/advanced-displacement-cost))

(provide 'day7)
