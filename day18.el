(require 'dash)
(require 's)
(require 'advent-utils)

(defun day18/read-number (line)
  (let ((clean-text (s-replace "," " "
                               (s-replace "]" ")"
                                          (s-replace "[" "(" line)))))
    (eval (car (read-from-string (concat "'" clean-text))))))

(defun day18/part-1 (lines)
  (-map #'day18/read-number lines)
  (error "Not yet implemented"))

(defun day18/part-2 (lines)
  (error "Not yet implemented"))

(provide 'day18)
