(require 'dash)
(require 'advent-utils)

(defun day17/read-target (line)
  (string-match "target area: x=\\([-0-9]+\\)\.\.\\([-0-9]+\\), y=\\([-0-9]+\\)\.\.\\([-0-9]+\\)" line)
  (list :x-min (string-to-number (match-string 1 line))
        :x-max (string-to-number (match-string 2 line))
        :y-min (string-to-number (match-string 3 line))
        :y-max (string-to-number (match-string 4 line))))

(defun day17/part-1 (line)
  (error "Not yet implemented"))

(defun day17/part-2 (line)
  (error "Not yet implemented"))

(provide 'day17)

(setq example "target area: x=20..30, y=-10..-5")
(setq problem "target area: x=282..314, y=-80..-45")
