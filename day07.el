(require 'dash)
(require 'advent-utils)

(defun day07/compute-displacement-cost (bins destination cost-formula)
  (let ((cost 0))
    (maphash (lambda (pos count)
               (setq cost (+ cost(* count (funcall cost-formula pos destination)))))
             bins)
    cost))

(defun day07/bin-crabs (crabs)
  "Save the number of crabs at each position in a table"
  (let ((bins (advent/table)))
    (--each crabs (advent/put bins it (1+ (advent/get bins it 0))))
    bins))

(defun day07/compute-all-costs (crabs cost-formula)
  "It naively computes the cost by adding the cost of each crab.

A smarter approach would be to bin the crabs first, an even
smarter one would be to figure the cost-formula out…"
  (let ((min-pos (apply #'min crabs))
        (max-pos (apply #'max crabs))
        (binned-crabs (day07/bin-crabs crabs)))
    (--map (cons it (day07/compute-displacement-cost binned-crabs it cost-formula))
           (number-sequence min-pos max-pos))))

(defun day07/compare--costs (cons-a cons-b)
  "cons-a and cons-b are cons of (index . cost) "
  (< (cdr cons-a)
     (cdr cons-b)))

(defun day07/simple-displacement-cost (p1 p2)
  (abs (- p1 p2)))

(defun day07/compute-most-efficient-displacement-cost (crabs cost-formula)
  (let ((binned-crabs (day07/bin-crabs crabs)))
   (cdr (advent/bogus-gradient (apply #'min crabs)
                           (apply #'max crabs)
                           (lambda (displacement)
                             (day07/compute-displacement-cost binned-crabs displacement cost-formula))))))

(defun day07/part-1 (crabs)
  (day07/compute-most-efficient-displacement-cost crabs #'day07/simple-displacement-cost))

(defun day07/advanced-displacement-cost (p1 p2)
  (let ((tmp (abs (- p1 p2))))
    (/ (* tmp (1+ tmp)) 2)))

(defun day07/part-2 (crabs)
  (day07/compute-most-efficient-displacement-cost crabs #'day07/advanced-displacement-cost))

(provide 'day07)
