(require 'dash)
(require 'advent-utils)

(defun day22/read-interval (token)
  (let ((as-list (-map #'string-to-number
                       (split-string (cadr (split-string token "=" t)) "[.]+" t))))
    (cons (car as-list)
          (cadr as-list))))

(defun day22/read-state (token)
  (cond
   ((string= token "on") 1)
   ((string= token "off") 0)
   (t (error "Unexpected token"))))

(defun day22/read-step (line)
  (let* ((state-coords (split-string line " " t))
         (coords (-map #'day22/read-interval (split-string (cadr state-coords) "," t))))
    (list coords (day22/read-state (car state-coords)))))

(defun day22/read-steps (lines)
  (-map #'day22/read-step lines))

(defun day22/part-1 (lines)
  (error "Not yet implemented"))

(defun day22/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day22)

(setq example (day22/read-steps (advent/read-problem-lines 22 :example)))
(setq problem (day22/read-steps (advent/read-problem-lines 22 :problem)))
