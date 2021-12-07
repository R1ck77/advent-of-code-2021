(require 'dash)
(require 'advent-utils)

;;; extract
(defun day6/read-crabs (line)
  (-map #'string-to-number (split-string line "," t)))

(defun day7/part-1 (lines)
  (error "Not yet implemented"))

(defun day7/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day7)
