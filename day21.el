(require 'dash)
(require 'advent-utils)

(defun day21/read-start-pos (line)
  (string-to-number (elt (split-string line) 4)))

(defun day21/read-input (lines)
  (vector (day21/read-start-pos (car lines))
          (day21/read-start-pos (cadr lines))))

(defun day21/part-1 (lines)
  (error "Not yet implemented"))

(defun day21/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day21)

(setq example (day21/read-input(advent/read-problem-lines 21 :example)))
(setq problem (day21/read-input(advent/read-problem-lines 21 :problem)))
