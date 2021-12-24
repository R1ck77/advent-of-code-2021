(require 'day23-s)
(require 'day23-l)

(defun day23/part-1 (lines)
  (day23-s/solution lines))

(defun day23/part-2 (lines)
  (day23-l/solution lines))

(provide 'day23)

(setq es (day23-s/s-read-problem (advent/read-problem-lines 23 :example 1)))
(setq ps (day23-s/s-read-problem (advent/read-problem-lines 23 :problem 1)))
(setq el (day23-l/l-read-problem (advent/read-problem-lines 23 :example 2)))
(setq pl (day23-l/l-read-problem (advent/read-problem-lines 23 :problem 2)))

(setq max-lisp-eval-depth 100000)
(setq max-specpdl-size 100000)
