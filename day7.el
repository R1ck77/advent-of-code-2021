(require 'dash)
(require 'advent-utils)

;;; extract
(defun day7/read-crabs (line)
  (-map #'string-to-number (split-string line "," t)))

(defun day7/check-displacement-cost (crabs pos)
  (apply #'+ (--map (abs (- it pos)) crabs)))

(defun day7/compute-all-costs (crabs)
  "It naively computes the cost by adding the cost of each crab.

A smarter approach would be to bin the crabs first, an even
smarter one would be to figure the formula out…"
  (let ((min-pos (apply #'min crabs))
        (max-pos (apply #'max crabs)))
    (--map (cons it (day7/check-displacement-cost crabs it))
           (number-sequence min-pos max-pos))))

(defun day7/compare--costs (cons-a cons-b)
  "cons-a and cons-b are cons of (index . cost) "
  (< (cdr cons-a)
     (cdr cons-b)))

(defun day7/part-1 (line)
  (cdar
   (-sort #'day7/compare--costs
          (day7/compute-all-costs (day7/read-crabs line)))))

(defun day7/part-2 (line)
  (let ((crabs (day7/read-crabs line)))      
    (error "Not yet implemented")))

(provide 'day7)
