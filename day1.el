(require 'dash)
(require 'advent-utils)

(defun count-increases (start list)
  (let ((count 0))
    (while list
      (if (> (car list) start)
          (setq count (1+ count)))
      (setq start (car list))
      (setq list (cdr list)))
    count))

(defun day1/part-1 (input)
  (count-increases (car input) (cdr input)))

(defun day1/part-2 (input)
  (error "Not implemented"))

(provide 'day1)
