(require 'dash)
(require 'advent-utils)

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

(provide 'day5)
