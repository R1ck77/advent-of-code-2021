(require 'dash)
(require 'advent-utils)

(defconst max-vy-search 1001)

(defun day17/read-target (line)
  (string-match "target area: x=\\([-0-9]+\\)\.\.\\([-0-9]+\\), y=\\([-0-9]+\\)\.\.\\([-0-9]+\\)" line)
  (list :x-min (string-to-number (match-string 1 line))
        :x-max (string-to-number (match-string 2 line))
        :y-min (string-to-number (match-string 3 line))
        :y-max (string-to-number (match-string 4 line))))


(defun day17/vertical-hit? (target vy)
  "Returns the maximum-position if the probe hits the mark, nil otherwise"
  (let ((overshot)
        (hit)
        (pos 0)
        (max-pos 0))
    (while (not (or overshot hit))
      (setq pos (+ pos vy))
      (setq max-pos (max max-pos pos))
      (setq vy (1- vy))
      (setq overshot (< pos (plist-get target :y-min)))
      (setq hit (and (not overshot)
                     (<= pos (plist-get target :y-max)))))
    (and (not overshot) (list vy max-pos))))

(defun day17/compute-max-vy (target top-vy)
  (cadr (car (nreverse (-non-nil 
                   (--map (day17/vertical-hit? target it)
                          (number-sequence 0 (or top-vy 1001))))))))

(defun day17/part-1 (line)
  "1001 is quite arbitrary: I have no idea whether a solution exists for vy = 1e39. Is there a mathematical proof?"
  (day17/compute-max-vy (day17/read-target line) max-vy-search))

(defun day17/part-2 (line)
  (error "Not yet implemented"))

(provide 'day17)

(setq example (day17/read-target "target area: x=20..30, y=-10..-5"))
(setq problem (day17/read-target "target area: x=282..314, y=-80..-45"))
