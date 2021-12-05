(require 'cl)
(require 'dash)

(defun advent/goto (x y)
  (goto-char 0)
  (move-to-column x t)
  (picture-move-down y))

(defun advent/table ()
  (make-hash-table :test #'equal))

(defun advent/put (table key value)
  (puthash key value table))

(defun advent/get (table key &optional default)
  (gethash key table default))

(defun advent/compute-input-name (day type)
  (format (cond
           ((eq type :example)
            "data/day%d-example.txt")
           ((eq type :problem)
            "data/day%d-problem.txt")
           (t (error "Unexpected problem type")))
          day))

(defun advent/read-problem-text (day type)
  (with-temp-buffer
    (insert-file-contents (advent/compute-input-name day type))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun advent/read-problem-lines (day type)
  (split-string (advent/read-problem-text day type) "\n" t))

(defun advent/read-problem-numbers (day type)
  (-map #'string-to-number (advent/read-problem-lines day type)))

(defun advent/read-problem-tokens (day type)
  (--map (split-string it " ") (advent/read-problem-lines day type)))

(defun advent/read-problem-instructions (day type)
  (--map (list (intern (concat ":" (car it)))
               (string-to-number (cadr it)))
         (advent/read-problem-tokens day type)))

(defun advent/read-raw-problem-lines (day type)
  (split-string (advent/read-problem-text day type) "\n"))

(defun advent/add-to-accumulator (acc line)
  (let ((groups (car acc))
        (current-group (cadr acc)))
    (if (/= (length line) 0)
        (list groups (cons line current-group))
      (list (cons (reverse current-group) groups) nil))))

(defun advent/group-lines (lines)
  (let ((accumulated (-reduce-from #'advent/add-to-accumulator
                                   '(() ())
                                   lines)))
    (if (cadr accumulated)
        (reverse (cons (reverse (cadr accumulated)) (car accumulated)))
      (reverse (car accumulated)))))

(defun advent/read-blocks-of-lines (day type)
  (advent/group-lines (advent/read-raw-problem-lines day type)))

(provide 'advent-utils)
