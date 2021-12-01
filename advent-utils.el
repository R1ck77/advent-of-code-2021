
(defun compute-input-name (day part type)
  (format (cond
           ((eq type :example)
            "data/day%d-example%d.txt")
           ((eq type :problem)
            "data/day%d-problem%d.txt")
           (t (error "Unexpected problem type")))
          day part))

(defun read-problem-lines (day part type)
  (split-string (with-temp-buffer
     (insert-file-contents (compute-input-name day part type))
     (buffer-substring-no-properties (point-min) (point-max)))))

(provide 'advent-utils)
