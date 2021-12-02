(require 'cl)

(defun compute-input-name (day type)
  (format (cond
           ((eq type :example)
            "data/day%d-example.txt")
           ((eq type :problem)
            "data/day%d-problem.txt")
           (t (error "Unexpected problem type")))
          day))

(defun read-problem-lines (day type)
  (let ((content (with-temp-buffer
                   (insert-file-contents (compute-input-name day type))
                   (buffer-substring-no-properties (point-min) (point-max)))))
    (split-string content "\n" t)))

(defun read-problem-numbers (day type)
  (-map #'string-to-number (read-problem-lines day type)))

(defun read-problem-tokens (day type)
  (--map (split-string it " ") (read-problem-lines day type)))

(defun read-problem-instructions (day type)
  (--map (list (intern (concat ":" (car it)))
               (string-to-number (cadr it)))
        (read-problem-tokens day type)))

(provide 'advent-utils)
