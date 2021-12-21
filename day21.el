(require 'dash)
(require 'advent-utils)

(defun day21/read-start-pos (line)
  (string-to-number (elt (split-string line) 4)))

(defun day21/read-input (lines)
  (vector (vector (day21/read-start-pos (car lines)) 0)
          (vector (day21/read-start-pos (cadr lines)) 0)))

(defun day21/create-det-dice ()
  (-partition 3 (-flatten (-repeat 3 (number-sequence 1 100)))))

(defun day21/next-det-roll (state)
  (list (car state)
        (let ((new-state (cdr state)))
          (or new-state (day21/create-det-dice)))))

(defun day21/move (position points)
  "Return the next position after a roll"
  (1+ (mod (+ (1- position) points) 10)))

(defun day21/move-player (position dice-state)
  (day21/))

(defun day21/game-step (players dice-state dice-rolls)

  )

(defun day21/part-1 (lines)
  (error "Not yet implemented"))

(defun day21/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day21)

(setq example (day21/read-input(advent/read-problem-lines 21 :example)))
(setq problem (day21/read-input(advent/read-problem-lines 21 :problem)))
