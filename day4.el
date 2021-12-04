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

(defun day4/part-1 (input)
  (error "Not yet implemented"))


(defun day4/part-2 (input)
  (error "Not yet implemented"))

(provide 'day4)
