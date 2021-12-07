(require 'dash)
(require 'advent-utils)

;; table: intorno a 1.3ms 13ms

;;; TODO/FIXME arrays were the way to go
(defun day6/read-fishes (line)
  (-map #'string-to-number (split-string line "," t)))

(defun day6/create-empty-counter ()
  "Create a counter with all slots reset to 0"
  (let ((table (advent/table)))
    (--each (number-sequence 0 8)
      (advent/put table it 0))
    table))

(defun day6/bin-fishes (fishes)
  "Create a counter with the starting status of the fishes"
  (let ((counter (day6/create-empty-counter)))
    (--each fishes (advent/update counter
                                  it
                                  (lambda (key old-value)
                                    (1+ old-value))))
    counter))

(defun day6/format-counter (counter)
  (let ((message ""))
   (--each (number-sequence 0 8)
     (setq message (concat message (format "%d -> %d\n" it (advent/get counter it)))))
   message))

(defun day6/increment-fishes (_ added base)
  (+ added base))

(defun day6/evolve (counter)
  "Return a new table with the structure"
  (let ((new-counter (day6/create-empty-counter)))
    ;; Reduce all "safe" timers, from 8 to 1 included
    (--each (number-sequence 1 8)
      (advent/put new-counter (1- it) (advent/get counter it)))
    ;; Generate new fishes
    (let ((0-fishes (advent/get counter 0)))
      ;; Add newly born fishes to 8
      (advent/update new-counter 8 #'day6/increment-fishes 0 0-fishes)
      ;; Reset the mother to 6
      (advent/update new-counter 6 #'day6/increment-fishes 0 0-fishes))
    new-counter))

(defun day6/count-fishes (counter)
  (let ((sum 0))
    (maphash (lambda (k v)
               (setq sum (+ sum v)))
             counter)
    sum))

(defun day6/part-1 (line)
  (day6/count-fishes
   (advent/iterate #'day6/evolve
                   (day6/bin-fishes (day6/read-fishes line))
                   80)))

(defun day6/part-2 (line)
  (day6/count-fishes
   (advent/iterate #'day6/evolve
                   (day6/bin-fishes (day6/read-fishes line))
                   256)))

(provide 'day6)
