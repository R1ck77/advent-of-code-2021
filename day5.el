(require 'dash)
(require 'advent-utils)
(require 'advent-extra-utils)

(defun day5/read-point (point-as-string)
  (let ((tokens (split-string point-as-string "," t)))
    (cons (string-to-number (car tokens))
          (string-to-number (cadr tokens)))))

(defun day5/line-to-points (line)
  (let ((tokens (split-string line)))
    (list (day5/read-point (elt tokens 0))
          (day5/read-point (elt tokens 2)))))

(defun day5/read-vents (lines)
  (-map #'day5/line-to-points lines))

(defun day5/diagonal-trace (line)  
  (let ((start (car line))
        (end (cadr line)))
    (let ((steps (1+
                  (max (abs (- (car end) (car start)))
                       (abs (- (cdr end) (cdr start))))))
            (dx (signum (- (car end) (car start))))
            (dy (signum (- (cdr end) (cdr start)))))
        (--iterate (cons (+ (car it) dx)
                         (+ (cdr it) dy))
                   start
                   steps))))

(defun day5/vertical? (line)
  (= (car (car line))
     (car (cadr line))))

(defun day5/horizontal? (line)
  (= (cdr (car line))
     (cdr (cadr line))))

(defun day5/aligned? (line)
  (or (day5/vertical? line)
      (day5/horizontal? line)))

(defun day5/compute-trace (line)
  (day5/diagonal-trace line))

(defun day5/write-trace-on-table (table point)
  (let ((current-value (advent/get table point 0)))
    (advent/put table point (1+ current-value))
    table))

(defun day5/accumulate-vents-on-table (vents)
  (let ((table (advent/table)))
    (--each (-map #'day5/compute-trace vents)
      (-reduce-from #'day5/write-trace-on-table table it))
    table))

(defun day5/count-dangerous-places (table)
  (let ((counter 0))
    (maphash (lambda (_ y)
               (if (> y 1)
                   (setq counter (1+ counter))))
             table)
    counter))

(defun day5/part-1 (lines)
  (day5/count-dangerous-places
   (day5/accumulate-vents-on-table (-filter #'day5/aligned? (day5/read-vents lines)))))

(defun day5/part-2 (lines)
  (day5/count-dangerous-places
   (day5/accumulate-vents-on-table (day5/read-vents lines))))

;;; The following code shows the traces on a buffer (second part only)

(defun day5/bump--value (x y)
  (advent/goto x y)
  (let ((here (point)))
   (if (= (line-end-position) here)
       (insert "1")
     (let ((current-raw-value (buffer-substring-no-properties here (1+ here))))
       (delete-char 1)
       (cond
        ((string= "*" current-raw-value) (insert "*"))
        ((string= " " current-raw-value) (insert "1"))
        (t (let ((current-value (string-to-number current-raw-value)))
           (if (= current-value 9)
               (insert "*")
             (insert (number-to-string (1+ current-value)))))))))))

(defun day5/accumulate--trace-on-buffer (trace)
  (--each trace
    (day5/bump--value (car it) (cdr it))))

(defun day5/reserve--space (max)
  "Reserve a rectangle on the screen big enough to contain all traces

It accounts for the different coordinates between buffer and vents"
  (save-excursion
    (advent/goto (1+ (car max)) (1+ (cdr max))))
  max)

(defun day5/compute--traces-boundary (all-traces)
  "Return the size of the minimum rectangle containing all traces"
  (--reduce-from (cons (max (car acc) (car it))
                       (max (cdr acc) (cdr it)))
                 '(0 . 0) (-flatten all-traces)))

(defun day5/accumulate--vents-on-buffer (vents)
  "Write the intersections in the current buffer"
  (let* ((all-traces (-map #'day5/compute-trace vents))
         (bounds (day5/compute--traces-boundary all-traces)))
    (day5/reserve--space bounds)
    (save-excursion
     (-each all-traces #'day5/accumulate--trace-on-buffer))))

(defun day5/show-intersections (lines)
  (let ((buffer (get-buffer-create "* Day 5 sample output *")))
    (switch-to-buffer buffer)
    (erase-buffer)
    (day5/accumulate--vents-on-buffer (day5/read-vents lines))))

(defun day5/show-part2-example ()
  (interactive)
  (day5/show-intersections (advent/read-problem-lines 5 :example)))

(defun day5/show-part2-problem ()
  "This operation may take some time (around a minute)"
  (interactive)
  (day5/show-intersections (advent/read-problem-lines 5 :problem)))

(provide 'day5)
