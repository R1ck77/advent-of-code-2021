
(defun compute-input-name (day type)
  (format (cond
           ((eq type :example)
            "data/day%d-example.txt")
           ((eq type :problem)
            "data/day%d-problem.txt")
           (t (error "Unexpected problem type")))
          day))

(defun read-problem-lines (day type)
  (split-string (with-temp-buffer
     (insert-file-contents (compute-input-name day type))
     (buffer-substring-no-properties (point-min) (point-max)))))

(defun read-problem-numbers (day type)
  (-map #'string-to-number (read-problem-lines day type)))

(provide 'advent-utils)
